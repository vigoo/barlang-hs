{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns #-}

module Language.Barlang.Compiler where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Binary.Builder
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Language.Bash as SH
import qualified Language.Bash.Annotations as SH
import qualified Language.Bash.Syntax as SH
import qualified Language.Bash.PrettyPrinter as SHPP
import Language.Barlang.Language
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
                   | InvalidParameterTypes SymbolName [Type] [ExtendedType]
                   | InvalidReturnType SymbolName Type ExtendedType
                   | InvalidParametersForRunStatement
                   | InvalidBooleanExpression
                   | InvalidTypesInNumericExpression [ExtendedType]
                   | EqualityUsedOnNonEqualTypes [ExtendedType]
                   | UnsupportedTypeInBooleanExpression ExtendedType
                     deriving (Show)

type CompilerMonad = ExceptT CompilerError (State Context)

data AssignedSymbol = AssignedSymbol { asName :: SymbolName
                                     , asIdString :: B.ByteString
                                     , asId :: SH.Identifier
                                     }
                    deriving (Show)

data ExtendedType = SimpleType Type
                  | TFunRef [Type] Type
                  deriving (Show)

class TypeEq a where
  typeEq :: a -> a -> Bool

instance TypeEq Type where
  typeEq = (==)

instance TypeEq ExtendedType where
  typeEq (SimpleType t1) (SimpleType t2) = t1 `typeEq` t2
  typeEq (SimpleType (TFun ps1 t1)) (TFunRef ps2 t2) = ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq (TFunRef ps1 t1) (SimpleType (TFun ps2 t2)) = ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq (TFunRef ps1 t1) (TFunRef ps2 t2) = ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq _ _ = False

instance (TypeEq a) => TypeEq [a] where
  typeEq [] [] = True
  typeEq (_:_) [] = False
  typeEq [] (_:_) = False
  typeEq (x:xs) (y:ys) = x `typeEq` y && xs `typeEq` ys

data Context = Context { ctxScope :: Scope
                       , ctxSymbols :: Map.Map SymbolName AssignedSymbol
                       , ctxSymbolTypes :: Map.Map SymbolName ExtendedType
                       , ctxLastTmp :: Int
                       }
             deriving (Show)

setSafe' :: SH.Statement t
setSafe' = SH.SimpleCommand "set" [ "-o", "nounset"
                                  , "-o", "pipefail"
                                  ]

toBash :: (SHPP.Annotation t) => SH.Statement t -> Builder
toBash st = mconcat [ fromByteString "#!/bin/bash\n"
                    , SHPP.builder (setSafe' :: SH.Statement ())
                    , fromByteString "\n\n"
                    , SHPP.builder st ]

compileToString :: Script -> String
compileToString script =
    case (evalState (runExceptT $ compile script) initialContext) of
      Right st -> BL.toString $ toLazyByteString $ toBash st
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

storeType :: Context -> SymbolName -> ExtendedType -> Context
storeType ctx@Context{..} sym typ = ctx { ctxSymbolTypes = Map.insert sym typ ctxSymbolTypes }

storeTypeM :: SymbolName -> ExtendedType -> CompilerMonad ()
storeTypeM sym typ = modify (\ctx -> storeType ctx sym typ)

findSymbol :: Context -> SymbolName -> Maybe AssignedSymbol
findSymbol Context{..} sym = Map.lookup sym ctxSymbols

findSymbolM :: SymbolName -> CompilerMonad (Maybe AssignedSymbol)
findSymbolM sym = gets $ \ctx -> findSymbol ctx sym

findType :: Context -> SymbolName -> Maybe ExtendedType
findType Context{..} sym = Map.lookup sym ctxSymbolTypes

findTypeM :: SymbolName -> CompilerMonad (Maybe ExtendedType)
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

compileTestExpr :: Expression -> CompilerMonad B.ByteString
compileTestExpr = \case
    EBoolLit True -> return "TRUE"
    EBoolLit False -> return "\"\""
    EVar sym -> do
      r <- findSymbolM sym
      t <- findTypeM sym
      case r of
       Just asym ->
         case t of
          Just (SimpleType TBool) ->
            return $ "( \"$" <> asIdString asym <> "\" = \"0\" )"
          Just (SimpleType TInt) ->
            return $ "\"" <> asIdString asym <> "\""
          Just (SimpleType TDouble) ->
            return $ "\"" <> asIdString asym <> "\""
          Just (SimpleType TString) ->
            return $ "\"" <> asIdString asym <> "\""
          Just t' ->
            throwError $ UnsupportedTypeInBooleanExpression t'
          Nothing ->
            throwError $ CannotInferType sym
       Nothing ->
         throwError $ UndefinedSymbol sym
    EAnd a b -> binaryTestExpr a b "&&"
    EOr a b -> binaryTestExpr a b "||"
    ENot e -> do
      e' <- compileTestExpr e
      return $ "! " <> e'
    EEq a b -> binaryTestExpr a b "="
    ENeq a b -> binaryTestExpr a b "!="

    _ -> throwError InvalidBooleanExpression
  where
    binaryTestExpr a b op = do
      e1 <- compileTestExpr a
      e2 <- compileTestExpr b
      return $ e1 <> " " <> op <> " " <> e2

compileBoolExpr :: Expression -> CompilerMonad B.ByteString
compileBoolExpr expr = case expr of
  EBoolLit True -> return "true"
  EBoolLit False -> return "false"
  EVar sym -> do
    r <- findSymbolM sym
    case r of
     Just asym ->
       return $ "$(exit $" <> asIdString asym <> ")"
     Nothing ->
       throwError $ UndefinedSymbol sym
  EAnd a b -> do
    e1 <- compileBoolExpr a
    e2 <- compileBoolExpr b
    return $ e1 <> " && " <> e2
  EOr a b -> do
    e1 <- compileBoolExpr a
    e2 <- compileBoolExpr b
    return $ e1 <> " || " <> e2
  _ -> do
    e' <- compileTestExpr expr
    return $ "[[ " <> e' <> " ]]"

boolTempVar :: Expression -> CompilerMonad (AssignedSymbol, CompilerMonad BashStatement)
boolTempVar expr = do
  boolExpr <- compileBoolExpr expr
  tmpSym <- generateTmpSym
  let assign = SH.Assign (SH.Var (asId tmpSym) (SH.ReadVar $ SH.VarSpecial SH.DollarQuestion))
  return (tmpSym, return $ SH.Sequence (SH.Annotated (SH.Lines [boolExpr] []) assign) (noAnnotation SH.Empty))

compileIntegerExpr :: Expression -> CompilerMonad B.ByteString
compileIntegerExpr expr = case expr of
  EIntLit n -> return $ B.fromString $ show n
  EAdd a b -> compileNumericBinaryOp a b "+"
  ESub a b -> compileNumericBinaryOp a b "-"
  EMul a b -> compileNumericBinaryOp a b "*"
  EDiv a b -> compileNumericBinaryOp a b "/"
  _ -> throwError $ NotSupported (show expr)
 where
   compileNumericBinaryOp a b op = do
    e1 <- compileIntegerExpr a
    e2 <- compileIntegerExpr b
    return $ "(" <> e1 <> ") " <> op <> " (" <> e2 <> ")"

integerTempVar :: Expression -> CompilerMonad (AssignedSymbol, CompilerMonad BashStatement)
integerTempVar expr = do
  numExpr <- compileIntegerExpr expr
  tmpSym <- generateTmpSym
  return (tmpSym, return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym) <> "=$((" <> numExpr <> "))"] []) SH.Empty) (noAnnotation SH.Empty))

compileDoubleExpr :: Expression -> CompilerMonad B.ByteString
compileDoubleExpr expr = case expr of
  EIntLit n -> return $ B.fromString $ show n
  EDoubleLit n -> return $ B.fromString $ show n
  EAdd a b -> compileNumericBinaryOp a b "+"
  ESub a b -> compileNumericBinaryOp a b "-"
  EMul a b -> compileNumericBinaryOp a b "*"
  EDiv a b -> compileNumericBinaryOp a b "/"
  _ -> throwError $ NotSupported (show expr)
 where
   compileNumericBinaryOp a b op = do
    e1 <- compileDoubleExpr a
    e2 <- compileDoubleExpr b
    return $ "(" <> e1 <> ") " <> op <> " (" <> e2 <> ")"

doubleTempVar :: Expression -> CompilerMonad (AssignedSymbol, CompilerMonad BashStatement)
doubleTempVar expr = do
  numExpr <- compileDoubleExpr expr
  tmpSym <- generateTmpSym
  return (tmpSym, return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym) <> "=$(bc -l <<< \"" <> numExpr <> "\")"] []) SH.Empty) (noAnnotation SH.Empty))

compileExpr :: Expression -> CompilerMonad (CompilerMonad BashStatement, BashExpression)
compileExpr expr =
    case expr of
      EStringLit str -> return (noPrereq, (SH.literal . B.fromString) str)

      EBoolLit False -> return (noPrereq, (SH.literal "1"))
      EBoolLit True -> return (noPrereq, (SH.literal "0"))

      EIntLit n -> return (noPrereq, (SH.literal (B.fromString $ show n)))

      EDoubleLit n -> return (noPrereq, (SH.literal (B.fromString $ show n)))

      EVar sym -> do
                r <- findSymbolM sym
                t <- findTypeM sym
                case r of
                  Just asym ->
                      case t of
                        Just (SimpleType (TFun _ _)) -> return (noPrereq, (SH.literal $ asIdString asym))
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

      EAnd _ _ -> boolSubExpr
      EOr _ _ -> boolSubExpr
      ENot _ -> boolSubExpr

      EAdd a b -> numericSubExpr a b
      ESub a b -> numericSubExpr a b
      EMul a b -> numericSubExpr a b
      EDiv a b -> numericSubExpr a b

      ELess _ _ -> throwError $ NotSupported "<"
      ELessEq _ _ -> throwError $ NotSupported "<="
      EGreater _ _ -> throwError $ NotSupported ">"
      EGreaterEq _ _ -> throwError $ NotSupported ">="

      EEq _ _ -> boolSubExpr
      ENeq _ _ -> boolSubExpr
  where
    boolSubExpr = do
      (tmpSym, prereq) <- boolTempVar expr
      return (prereq, SH.ReadVar (SH.VarIdent $ asId tmpSym))

    integerSubExpr = do
      (tmpSym, prereq) <- integerTempVar expr
      return (prereq, SH.ReadVar (SH.VarIdent $ asId tmpSym))

    doubleSubExpr = do
      (tmpSym, prereq) <- doubleTempVar expr
      return (prereq, SH.ReadVar (SH.VarIdent $ asId tmpSym))

    numericSubExpr a b = do
      ta <- typeCheckExpr a
      tb <- typeCheckExpr b
      case (ta, tb) of
       (SimpleType TDouble, _) -> doubleSubExpr
       (_, SimpleType TDouble) -> doubleSubExpr
       _ -> integerSubExpr

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


typeCheckExpr :: Expression -> CompilerMonad ExtendedType
typeCheckExpr expr =
    case expr of
      EStringLit _ -> return $ SimpleType TString

      EBoolLit _ -> return $ SimpleType TBool

      EIntLit _ -> return $ SimpleType TInt

      EDoubleLit _ -> return $ SimpleType TDouble

      EVar sym -> do
                t <- findTypeM sym
                case t of
                  Just typ -> return typ
                  Nothing -> throwError $ CannotInferType sym

      ESysVar _ -> return $ SimpleType TString

      EApply funRefExpr params -> do
          let funName = (show funRefExpr)
          paramTypes <- mapM typeCheckExpr params
          fnType <- typeCheckExpr funRefExpr
          case fnType of
            SimpleType (TFun expectedTypes retType) | (map SimpleType expectedTypes) `typeEq` paramTypes -> return $ SimpleType retType
                                                    | otherwise -> throwError $ InvalidParameterTypes funName expectedTypes paramTypes
            TFunRef expectedTypes retType | (map SimpleType expectedTypes) `typeEq` paramTypes -> return $ SimpleType retType
                                          | otherwise -> throwError $ InvalidParameterTypes funName expectedTypes paramTypes
            _ -> throwError $ SymbolNotBoundToFunction funName

      ENot e -> do
        t <- typeCheckExpr e
        case t of
         SimpleType TBool -> return $ SimpleType TBool
         _ -> throwError $ InvalidBooleanExpression

      EAnd e1 e2 -> typeCheckBinaryBoolExpr e1 e2
      EOr e1 e2 -> typeCheckBinaryBoolExpr e1 e2

      EAdd e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      ESub e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EMul e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EDiv e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing

      ELess e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      ELessEq e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      EGreater e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      EGreaterEq e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool

      EEq e1 e2 -> typeCheckEqualityExpr e1 e2
      ENeq e1 e2 -> typeCheckEqualityExpr e1 e2

  where
    typeCheckBinaryBoolExpr e1 e2 = do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2
      case (t1, t2) of
       (SimpleType TBool, SimpleType TBool) -> return $ SimpleType TBool
       _ -> throwError $ InvalidBooleanExpression

    typeCheckBinaryNumericExpr e1 e2 rt = do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2
      case (t1, t2) of
       (SimpleType TInt, SimpleType TInt) -> return $ SimpleType $ fromMaybe TInt rt
       (SimpleType TDouble, SimpleType TDouble) -> return $ SimpleType $ fromMaybe TDouble rt
       (SimpleType TInt, SimpleType TDouble) -> return $ SimpleType $ fromMaybe TDouble rt
       (SimpleType TDouble, SimpleType TInt) -> return $ SimpleType $ fromMaybe TDouble rt
       _ -> throwError $ InvalidTypesInNumericExpression [t1, t2]

    typeCheckEqualityExpr e1 e2 = do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2
      case t1 `typeEq` t2 of
       True -> return $ SimpleType TBool
       False -> throwError $ EqualityUsedOnNonEqualTypes [t1, t2]

funType :: [ParamDef] -> Type -> Type
funType params rettype = TFun (map (\case { ParamDef (_, t) -> t }) params) rettype

funToFunRef :: Type -> ExtendedType
funToFunRef (TFun ps t) = TFunRef ps t
funToFunRef t = SimpleType t

funScope :: Scope -> SymbolName -> Scope
funScope = (++)

funContext :: Context -> SymbolName -> [ParamDef] -> Context
funContext ctx@Context{..} name params = paramCtx { ctxSymbolTypes = ctxSymbolTypes `Map.union` paramSymbolTypes }
    where
      scope = funScope ctxScope name
      paramCtx = foldl (\c pd -> let ParamDef (n, _) = pd in fst $ createIdentifier c n) (ctx { ctxScope = scope }) params
      paramSymbolTypes = foldl (\m pd -> let ParamDef (n, t) = pd in Map.insert n (funToFunRef t) m) Map.empty params

funContextM :: SymbolName -> [ParamDef] -> CompilerMonad Context
funContextM name params = gets $ \ctx -> funContext ctx name params

typeCheckSt :: Statement -> CompilerMonad ExtendedType
typeCheckSt st =
    case st of
      SNoOp -> return $ SimpleType TUnit

      SVarDecl sym expr -> do
          (!typ) <- typeCheckExpr expr
          storeTypeM sym typ
          return $ SimpleType TUnit

      SDefFun sym pdef rettype stIn -> do
          storeTypeM sym (SimpleType $ funType pdef rettype)
          funCtx <- funContextM sym pdef
          (!sttype) <- runChildContext funCtx $ typeCheckSt stIn
          if sttype `typeEq` (SimpleType rettype)
          then return $ SimpleType TUnit
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
          if (typProgram `typeEq` (SimpleType TString)) && all (\t -> t `typeEq` (SimpleType TString)) typParams
          then return $ SimpleType TUnit -- TODO
          else throwError InvalidParametersForRunStatement
