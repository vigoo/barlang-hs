{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns #-}

module Language.Mes.Compiler where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Binary.Builder
import qualified Data.Map as Map
import Data.Maybe
import qualified Language.Bash as SH
import qualified Language.Bash.Annotations as SH
import qualified Language.Bash.Syntax as SH
import Language.Mes.Language
import Text.Printf

type Scope = String
type BashStatement = SH.Statement SH.Lines
type BashExpression = SH.Expression SH.Lines

data CompilerError = InvalidFunctionContext
                   | UndefinedSymbol SymbolName
                   | NotSupported String
                   | SymbolAlreadyDefined SymbolName
                   | CannotInferType SymbolName
                   | SymbolNotBoundToFunction SymbolName
                   | InvalidParameterTypes SymbolName [Type] [Type]
                   | InvalidReturnType SymbolName Type Type
                   | InvalidParametersForRunStatement
                     deriving (Show)

type CompilerMonad = ExceptT CompilerError (State Context)

data AssignedSymbol = AssignedSymbol { asName :: SymbolName
                                     , asIdString :: B.ByteString
                                     , asId :: SH.Identifier
                                     }
                    deriving (Show)

data Context = Context { ctxScope :: Scope
                       , ctxSymbols :: Map.Map SymbolName AssignedSymbol
                       , ctxSymbolTypes :: Map.Map SymbolName Type
                       , ctxLastTmp :: Int
                       }
             deriving (Show)

compileToString :: Script -> String
compileToString script =
    case (evalState (runExceptT $ compile script) initialContext) of
      Right st -> BL.toString $ toLazyByteString $ SH.script st
      Left err -> error (show err)

compile :: Script -> CompilerMonad BashStatement
compile Script{..} = do  _ <- typeCheckSt sStatement
                         compileSt sStatement

runChildContext :: Context -> CompilerMonad a -> CompilerMonad a
runChildContext ctx' f = let res = (evalState (runExceptT f) ctx')
                         in case res of
                              Right v -> return v
                              Left err -> throwError err

noAnnotation :: BashStatement -> SH.Annotated SH.Lines
noAnnotation = SH.Annotated (SH.Lines [] [])

initialContext :: Context
initialContext = Context { ctxScope = "_"
                         , ctxSymbols = Map.empty
                         , ctxSymbolTypes = Map.empty
                         , ctxLastTmp = 0
                         }

retVarName :: Context -> String
retVarName Context{..} = ctxScope ++ "__retvar"

retVarId :: Context -> SH.Identifier
retVarId ctx = SH.Identifier $ B.fromString $ retVarName ctx

retValName :: Context -> String
retValName Context{..} = ctxScope ++ "__retval"

retValId :: Context -> SH.Identifier
retValId ctx = SH.Identifier $ B.fromString $ retValName ctx

createIdentifier :: Context -> SymbolName -> (Context, AssignedSymbol)
createIdentifier ctx@Context{..} sym = (ctx { ctxSymbols = Map.insert sym asym ctxSymbols }, asym)
    where
      idString = B.fromString $ printf "%s_%s" ctxScope sym
      asym = AssignedSymbol sym idString $ (SH.Identifier idString)

createIdentifierM :: SymbolName -> CompilerMonad AssignedSymbol
createIdentifierM sym = do
  ctx <- get
  let (ctx', asym) = createIdentifier ctx sym
  put ctx'
  return asym

storeType :: Context -> SymbolName -> Type -> Context
storeType ctx@Context{..} sym typ = ctx { ctxSymbolTypes = Map.insert sym typ ctxSymbolTypes }

storeTypeM :: SymbolName -> Type -> CompilerMonad ()
storeTypeM sym typ = modify (\ctx -> storeType ctx sym typ)

findSymbol :: Context -> SymbolName -> Maybe AssignedSymbol
findSymbol Context{..} sym = Map.lookup sym ctxSymbols

findSymbolM :: SymbolName -> CompilerMonad (Maybe AssignedSymbol)
findSymbolM sym = gets $ \ctx -> findSymbol ctx sym

findType :: Context -> SymbolName -> Maybe Type
findType Context{..} sym = Map.lookup sym ctxSymbolTypes

findTypeM :: SymbolName -> CompilerMonad (Maybe Type)
findTypeM sym = gets $ \ctx -> findType ctx sym

generateTmpSym :: CompilerMonad AssignedSymbol
generateTmpSym = do
  ctx <- get
  let next = 1 + (ctxLastTmp ctx)
      idString = B.fromString $ printf "%s_tmp%d" (ctxScope ctx) next
  put $ ctx { ctxLastTmp = next }
  return $ AssignedSymbol (printf "tmp%d" next) idString $ SH.Identifier idString

withFunctionHeader :: Context -> [ParamDef] -> Type -> BashStatement -> BashStatement
withFunctionHeader ctx@Context{..} params rettype stIn = SH.Sequence (noAnnotation funHeader) (noAnnotation stIn)
    where
      startIdx = if rettype == TUnit then 1 else 2
      paramCount = length params
      assignParam :: SymbolName -> Int -> BashStatement
      assignParam n i = case findSymbol ctx n of
                          Just asym -> SH.Local $ SH.Var (asId asym) (SH.ReadVar $ SH.VarSpecial . fromJust . SH.specialVar $ B.fromString ("$" ++ (show i)))
                          Nothing -> error "Function context is not initialized properly"
      declRetVar = if rettype == TUnit
                   then SH.Empty
                   else SH.Local $ SH.Var (retVarId ctx) $ (SH.ReadVar $ SH.VarSpecial SH.Dollar1)
      decls = map (\case { (ParamDef (n, _), i) -> assignParam n i }) (params `zip` [startIdx..(startIdx+paramCount-1)])
      funHeader = foldl (\s1 s2 -> SH.Sequence (noAnnotation s1) (noAnnotation s2)) declRetVar decls

withPrereqs :: CompilerMonad BashStatement -> BashStatement -> CompilerMonad BashStatement
withPrereqs prereqs st = do
  pst <- prereqs
  return $ SH.Sequence (noAnnotation pst) (noAnnotation st)

noPrereq :: CompilerMonad BashStatement
noPrereq = return SH.Empty

compileExpr :: Expression -> CompilerMonad (CompilerMonad BashStatement, BashExpression)
compileExpr expr =
    case expr of
      EStringLit str -> return (noPrereq, (SH.literal . B.fromString) str)

      EVar sym -> do
                r <- findSymbolM sym
                t <- findTypeM sym
                case r of
                  Just asym ->
                      case t of
                        Just (TFun _ _) -> return (noPrereq, (SH.literal $ asIdString asym))
                        _ ->  return (noPrereq, SH.ReadVar (SH.VarIdent $ asId asym))
                  Nothing -> throwError $ UndefinedSymbol sym

      ESysVar sym -> return (noPrereq, SH.ReadVar (SH.VarIdent ((SH.Identifier . B.fromString) sym)))

      EApply funRefExpr params -> do
                (prereq1, funRef) <- compileExpr funRefExpr
                (prereqs, cParams) <- seqCompileExpr prereq1 params
                tmpSym <- generateTmpSym
                let finalPrereqs = withPrereqs prereqs $ SH.Sequence
                                                          (noAnnotation $ SH.Assign $ SH.Var (asId tmpSym) (SH.literal ""))
                                                          (noAnnotation $ SH.SimpleCommand funRef $ (SH.literal (asIdString tmpSym)):cParams)
                return (finalPrereqs, SH.ReadVar (SH.VarIdent $ asId tmpSym))

seqCompileExpr :: (CompilerMonad BashStatement) -> [Expression] -> CompilerMonad (CompilerMonad BashStatement, [BashExpression])
seqCompileExpr p e = seqCompileExpr' p e []
    where
      seqCompileExpr' :: (CompilerMonad BashStatement) -> [Expression] -> [BashExpression] -> CompilerMonad (CompilerMonad BashStatement, [BashExpression])
      seqCompileExpr' prereq [] rs = return (prereq, reverse rs)
      seqCompileExpr' prereq (expr:exprs) rs = do
        (prereq', res) <- compileExpr expr
        seqCompileExpr' (do pst <- prereq
                            pst' <- prereq'
                            return $ SH.Sequence (noAnnotation pst) (noAnnotation pst')
                        ) exprs (res:rs)

compileSt :: Statement -> CompilerMonad BashStatement
compileSt st =
    case st of
      SNoOp -> return SH.Empty

      SVarDecl sym expr -> do
              r <- findSymbolM sym
              case r of
                Just _ -> throwError $ SymbolAlreadyDefined sym
                Nothing -> do
                       asym <- createIdentifierM sym
                       (prereqs, cExpr) <- compileExpr expr
                       withPrereqs prereqs $ SH.Assign $ SH.Var (asId asym) cExpr

      SDefFun sym pdef rettype stIn -> do
              r <- findSymbolM sym
              case r of
                Just _ -> throwError $ SymbolAlreadyDefined sym
                Nothing -> do
                       asym <- createIdentifierM sym
                       funCtx <- funContextM sym pdef
                       cStIn <- runChildContext funCtx $ compileSt stIn
                       return $ SH.Function (SH.Simple $ asId asym) (noAnnotation $ withFunctionHeader funCtx pdef rettype cStIn)

      SSequence s1 s2 -> do
              t1 <- compileSt s1
              t2 <- compileSt s2
              return $ SH.Sequence (noAnnotation t1) (noAnnotation t2)

      SCall funRefExpr params -> do
              (prereq1, funRef) <- compileExpr funRefExpr
              (prereqs, cParams) <- seqCompileExpr prereq1 params
              withPrereqs prereqs $ SH.SimpleCommand funRef cParams

      SReturn expr -> do
              (prereq, cExpr) <- compileExpr expr
              ctx <- get
              let setRetVal = SH.Local (SH.Var (retValId ctx) cExpr)
                  passToCaller = B.fromString $ printf "eval $%s=\"$%s\"" (retVarName ctx) (retValName ctx)
              withPrereqs prereq $ SH.Sequence (SH.Annotated (SH.Lines [] [passToCaller]) setRetVal) (noAnnotation SH.Empty)

      SRun program params -> do
              (prereq1, cProgram) <- compileExpr program
              (prereqs, cParams) <- seqCompileExpr prereq1 params
              withPrereqs prereqs $ SH.SimpleCommand cProgram cParams


typeCheckExpr :: Expression -> CompilerMonad Type
typeCheckExpr expr =
    case expr of
      EStringLit _ -> return TString

      EVar sym -> do
                t <- findTypeM sym
                case t of
                  Just typ -> return typ
                  Nothing -> throwError $ CannotInferType sym

      ESysVar _ -> return TString

      EApply funRefExpr params -> do
          let funName = (show funRefExpr)
          paramTypes <- mapM typeCheckExpr params
          fnType <- typeCheckExpr funRefExpr
          case fnType of
            TFun expectedTypes retType | expectedTypes == paramTypes -> return retType
                                       | otherwise -> throwError $ InvalidParameterTypes funName expectedTypes paramTypes
            _ -> throwError $ SymbolNotBoundToFunction funName

funType :: [ParamDef] -> Type -> Type
funType params rettype = TFun (map (\case { ParamDef (_, t) -> t }) params) rettype

funScope :: Scope -> SymbolName -> Scope
funScope = (++)

funContext :: Context -> SymbolName -> [ParamDef] -> Context
funContext ctx@Context{..} name params = paramCtx { ctxSymbolTypes = ctxSymbolTypes `Map.union` paramSymbolTypes }
    where
      scope = funScope ctxScope name
      paramCtx = foldl (\c pd -> let ParamDef (n, _) = pd in fst $ createIdentifier c n) (ctx { ctxScope = scope }) params
      paramSymbolTypes = foldl (\m pd -> let ParamDef (n, t) = pd in Map.insert n t m) Map.empty params

funContextM :: SymbolName -> [ParamDef] -> CompilerMonad Context
funContextM name params = gets $ \ctx -> funContext ctx name params

typeCheckSt :: Statement -> CompilerMonad Type
typeCheckSt st =
    case st of
      SNoOp -> return TUnit

      SVarDecl sym expr -> do
          (!typ) <- typeCheckExpr expr
          storeTypeM sym typ
          return TUnit

      SDefFun sym pdef rettype stIn -> do
          storeTypeM sym (funType pdef rettype)
          funCtx <- funContextM sym pdef
          (!sttype) <- runChildContext funCtx $ typeCheckSt stIn
          if sttype == rettype
          then return TUnit
          else throwError $ InvalidReturnType sym rettype sttype

      SSequence s1 s2 -> typeCheckSt s1 >> typeCheckSt s2

      SCall funNameExpr params -> do
          (!typ) <- typeCheckExpr $ EApply funNameExpr params
          return typ

      SReturn expr -> do
          (!typ) <- typeCheckExpr expr
          return typ

      SRun program params -> do
          (!typProgram) <- typeCheckExpr program
          typParams <- mapM typeCheckExpr params
          if (typProgram == TString) && all (\t -> t == TString) typParams
          then return  TUnit -- TODO
          else throwError InvalidParametersForRunStatement
