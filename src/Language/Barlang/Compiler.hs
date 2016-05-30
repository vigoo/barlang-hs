{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Barlang.Compiler where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer.Lazy
import           Data.Binary.Builder
import qualified Data.ByteString.Lazy.UTF8      as BL
import qualified Data.ByteString.UTF8           as B
import           Data.Function
import qualified Data.Map                       as Map
import           Data.Maybe
import qualified Data.Set                       as Set
import           Debug.Trace
import           Language.Barlang.CompilerTypes
import           Language.Barlang.Language
import           Language.Barlang.Predefined
import qualified Language.Bash                  as SH
import qualified Language.Bash.PrettyPrinter    as SHPP
import qualified Language.Bash.Syntax           as SH
import           Text.Printf
import           Text.ShellEscape

class TypeEq a where
  typeEq :: a -> a -> Bool

instance TypeEq Type where
  typeEq = (==)

instance TypeEq TypeParam where
  typeEq = (==)

instance TypeEq ExtendedType where
  typeEq (SimpleType t1) (SimpleType t2) = t1 `typeEq` t2
  typeEq (SimpleType (TFun tps1 ps1 t1)) (TFunRef tps2 ps2 t2) = tps1 `typeEq` tps2 && ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq (TFunRef tps1 ps1 t1) (SimpleType (TFun tps2 ps2 t2)) = tps1 `typeEq` tps2 && ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq (TFunRef tps1 ps1 t1) (TFunRef tps2 ps2 t2) = tps1 `typeEq` tps2 && ps1 `typeEq` ps2 && t1 `typeEq` t2
  typeEq _ _ = False

instance (TypeEq a) => TypeEq [a] where
  typeEq [] [] = True
  typeEq (_:_) [] = False
  typeEq [] (_:_) = False
  typeEq (x:xs) (y:ys) = x `typeEq` y && xs `typeEq` ys

compileExpression :: (Expression -> ExpressionCompilerMonad BashExpression)
                  -> (BashExpression -> CompilerMonad BashStatement)
                  -> Expression
                  -> CompilerMonad BashStatement
compileExpression fn res expr = do (e, prereqs) <- runWriterT (fn expr)
                                   s <- res e
                                   prereqs <> return s

compileExpression' :: Expression
                   -> (Expression -> ExpressionCompilerMonad BashExpression)
                   -> (BashExpression -> CompilerMonad BashStatement)
                   -> CompilerMonad BashStatement
compileExpression' expr fn res = compileExpression fn res expr

compileExpressions :: (Expression -> ExpressionCompilerMonad BashExpression)
                   -> ([BashExpression] -> CompilerMonad BashStatement)
                   -> [Expression]
                   -> CompilerMonad BashStatement
compileExpressions fn res exprs = do partials <- mapM (runWriterT . fn) exprs
                                     s <- res (map fst partials)
                                     mconcat (map snd partials) <> return s

compileExpressions' :: [Expression]
                    -> (Expression -> ExpressionCompilerMonad BashExpression)
                    -> ([BashExpression] -> CompilerMonad BashStatement)
                    -> CompilerMonad BashStatement
compileExpressions' exprs fn res = compileExpressions fn res exprs

setSafe' :: SH.Statement t
setSafe' = SH.SimpleCommand "set" [ "-o", "nounset"
                                  , "-o", "pipefail"
                                  ]

toBash :: (SHPP.Annotation t) => SH.Statement t -> Builder
toBash st = mconcat [ fromByteString "#!/bin/bash\n"
                    , SHPP.builder (setSafe' :: SH.Statement ())
                    , fromByteString "\n\n"
                    , SHPP.builder st ]

compileToString :: Script -> Either CompilerError String
compileToString script =
    case evalState (runExceptT $ compile script) initialContext of
      Right st -> Right $ BL.toString $ toLazyByteString $ toBash st
      Left err -> Left err

compile :: Script -> CompilerMonad BashStatement
compile Script{..} = do  let replacedSt = replacePredefs sStatement
                         _ <- trace "type checking..." $ typeCheckSt replacedSt -- TODO: remove trace, add proper logging
                         trace "compiling..." $ compileSt replacedSt

replacePredefs :: Statement -> Statement
replacePredefs (SVarDecl n x) = SVarDecl n (replacePredefsExpr x)
replacePredefs (SDefFun n props tps ps rt st) = SDefFun n props tps ps rt (replacePredefs st)
replacePredefs (SSequence s1 s2) = SSequence (replacePredefs s1) (replacePredefs s2)
replacePredefs (SCall e0 es) = SCall (replacePredefsExpr e0) (map replacePredefsExpr es)
replacePredefs (SRun e0 es) = SRun (replacePredefsExpr e0) (map replacePredefsExpr es)
replacePredefs (SReturn e) = SReturn (replacePredefsExpr e)
replacePredefs (SIf cond tbody fbody) = SIf (replacePredefsExpr cond) (replacePredefs tbody) (replacePredefs fbody)
replacePredefs (SWhile cond body) = SWhile (replacePredefsExpr cond) (replacePredefs body)
replacePredefs (SUpdateVar n x) = SUpdateVar n (replacePredefsExpr x)
replacePredefs (SUpdateCell n i x) = SUpdateCell n (replacePredefsExpr i) (replacePredefsExpr x)
replacePredefs (SArrayDecl n t) = SArrayDecl n t
replacePredefs SNoOp = SNoOp

replacePredefsExpr :: Expression -> Expression
replacePredefsExpr (EVar n) | n `Map.member` predefined =
  EPredefined n
replacePredefsExpr (EApply e0 es) = EApply (replacePredefsExpr e0) (map replacePredefsExpr es)
replacePredefsExpr (EUnaryOp op e) = EUnaryOp op (replacePredefsExpr e)
replacePredefsExpr (EBinOp op a b) = EBinOp op (replacePredefsExpr a) (replacePredefsExpr b)
replacePredefsExpr (EArrayAccess a b) = EArrayAccess a (replacePredefsExpr b)
replacePredefsExpr e = e

initialContext :: Context
initialContext = Context { ctxScope = ""
                         , ctxSymbols = Map.empty
                         , ctxSymbolTypes = (SimpleType . predefType) `Map.map` predefined
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
      asym = AssignedSymbol sym idString $ SH.Identifier idString

createIdentifierM :: SymbolName -> CompilerMonad AssignedSymbol
createIdentifierM sym = do
  ctx <- get
  let (ctx', asym) = createIdentifier ctx sym
  put ctx'
  return asym

withFunctionHeader :: Context -> [ParamDef] -> Type -> BashStatement -> BashStatement
withFunctionHeader ctx@Context{..} params rettype stIn = SH.Sequence (noAnnotation funHeader) (noAnnotation stIn)
    where
      startIdx = if rettype == TUnit then 1 else 2
      paramCount = length params
      assignParam :: SymbolName -> Int -> BashStatement
      assignParam n i = case findSymbol ctx n of
                          Just asym -> SH.Local $ SH.Var (asId asym) (SH.ReadVar $ SH.VarSpecial . fromJust . SH.specialVar $ B.fromString ("$" ++ show i))
                          Nothing -> error "Function context is not initialized properly"
      declRetVar = if rettype == TUnit
                   then SH.Empty
                   else SH.Local $ SH.Var (retVarId ctx) $ SH.ReadVar $ SH.VarSpecial SH.Dollar1
      decls = map (\case { (ParamDef (n, _), i) -> assignParam n i }) (params `zip` [startIdx..(startIdx+paramCount-1)])
      funHeader = foldl (SH.Sequence `on` noAnnotation) declRetVar decls

compileTestExpr :: Expression -> ExpressionCompilerMonad B.ByteString
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
            return $ "\"$" <> asIdString asym <> "\""
          Just (SimpleType TDouble) ->
            return $ "\"$" <> asIdString asym <> "\""
          Just (SimpleType TString) ->
            return $ "\"$" <> asIdString asym <> "\""
          Just t' ->
            throwError $ UnsupportedTypeInBooleanExpression t'
          Nothing -> do
            dumpState
            throwError $ CannotInferType sym
       Nothing ->
         throwError $ UndefinedSymbol sym
    EStringLit str ->
        return $ "\"" <> B.fromString str <> "\""
    EBinOp BOAnd a b -> binaryTestExpr a b "-a"
    EBinOp BOOr a b -> binaryTestExpr a b "-o"
    EUnaryOp UONot e -> do
      e' <- compileTestExpr e
      return $ "! " <> e'
    EBinOp BOEq a b -> binaryTestExpr a b "-eq"
    EBinOp BONeq a b -> binaryTestExpr a b "-ne"
    EBinOp BOLess a b -> binaryTestExpr a b "-lt"
    EBinOp BOLessEq a b -> binaryTestExpr a b "-le"
    EBinOp BOGreater a b -> binaryTestExpr a b "-gt"
    EBinOp BOGreaterEq a b -> binaryTestExpr a b "-ge"

    e -> do
      nr <- numericSubExpr e
      case nr of
       Just (SH.ReadVar (SH.VarIdent (SH.Identifier tmpSymId))) ->
         return $ "\"" <> (B.fromString $ show tmpSymId) <> "\""
       Just (SH.Literal l) ->
         return $ "\"" <> unescape l <> "\""
       _ -> throwError $ InvalidBooleanExpression $ Just e
  where
    binaryTestExpr a b op = do
      e1 <- compileTestExpr a
      e2 <- compileTestExpr b
      return $ e1 <> " " <> op <> " " <> e2

compileBoolExpr :: Expression -> ExpressionCompilerMonad B.ByteString
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
  EBinOp BOAnd a b -> do
    e1 <- compileBoolExpr a
    e2 <- compileBoolExpr b
    return $ e1 <> " && " <> e2
  EBinOp BOOr a b -> do
    e1 <- compileBoolExpr a
    e2 <- compileBoolExpr b
    return $ e1 <> " || " <> e2
  _ -> do
    e' <- compileTestExpr expr
    return $ "[ " <> e' <> " ]"

boolTempVar :: Expression -> ExpressionCompilerMonad AssignedSymbol
boolTempVar expr = do
  boolExpr <- compileBoolExpr expr
  tmpSym <- generateTmpSym
  let assign = SH.Assign (SH.Var (asId tmpSym) (SH.ReadVar $ SH.VarSpecial SH.DollarQuestion))
  prereq $ return $ SH.Sequence (SH.Annotated (SH.Lines [boolExpr] []) assign) (noAnnotation SH.Empty)
  return tmpSym

compileIntegerExpr :: Expression -> ExpressionCompilerMonad B.ByteString
compileIntegerExpr expr = case expr of
  EIntLit n -> return $ B.fromString $ show n
  EBinOp BOAdd a b -> compileNumericBinaryOp a b "+"
  EBinOp BOSub a b -> compileNumericBinaryOp a b "-"
  EBinOp BOMul a b -> compileNumericBinaryOp a b "*"
  EBinOp BODiv a b -> compileNumericBinaryOp a b "/"
  EBinOp BOMod a b -> compileNumericBinaryOp a b "%"
  EApply _ _ -> do
    cExpr <- compileExpr expr
    tmpSym <- generateTmpSym
    prereq $ return $ SH.Sequence
      (noAnnotation $ SH.Assign $ SH.Var (asId tmpSym) cExpr) (noAnnotation SH.Empty)
    return $ "$" <> asIdString tmpSym
  EVar sym -> do
    r <- findSymbolM sym
    case r of
     Just asym ->
       return $ "$" <> asIdString asym
     Nothing ->
       throwError $ UndefinedSymbol sym
  _ -> throwError $ NotSupported (show expr)
 where
   compileNumericBinaryOp a b op = do
    e1 <- compileIntegerExpr a
    e2 <- compileIntegerExpr b
    return $ "(" <> e1 <> ") " <> op <> " (" <> e2 <> ")"

integerTempVar :: Expression -> ExpressionCompilerMonad AssignedSymbol
integerTempVar expr = do
  numExpr <- compileIntegerExpr expr
  tmpSym <- generateTmpSym
  prereq $ return $ SH.Sequence (SH.Annotated (SH.Lines [asIdString tmpSym <> "=$((" <> numExpr <> "))"] []) SH.Empty) (noAnnotation SH.Empty)
  return tmpSym

compileDoubleExpr :: Expression -> ExpressionCompilerMonad B.ByteString
compileDoubleExpr expr = case expr of
  EIntLit n -> return $ B.fromString $ show n
  EDoubleLit n -> return $ B.fromString $ show n
  EBinOp BOAdd a b -> compileNumericBinaryOp a b "+"
  EBinOp BOSub a b -> compileNumericBinaryOp a b "-"
  EBinOp BOMul a b -> compileNumericBinaryOp a b "*"
  EBinOp BODiv a b -> compileNumericBinaryOp a b "/"
  EApply _ _ -> extractSubExpression expr
  EArrayAccess _ _ -> extractSubExpression expr
  EPredefined sym ->
    case Map.lookup sym predefined of
      Just (PredefinedValue{..}) ->
        case predefValue of
          PredefinedExpression e -> extractSubExpression e
          CustomExpression _ -> throwError $ InvalidUseOfPredefinedFunction sym
      Nothing -> throwError $ UndefinedSymbol sym
  EVar sym -> do
    r <- findSymbolM sym
    case r of
     Just asym ->
       return $ "$" <> asIdString asym
     Nothing ->
       throwError $ UndefinedSymbol sym
  _ -> throwError $ NotSupported (show expr)
 where
   compileNumericBinaryOp a b op = do
    e1 <- compileDoubleExpr a
    e2 <- compileDoubleExpr b
    return $ "(" <> e1 <> ") " <> op <> " (" <> e2 <> ")"

   extractSubExpression e = do
     cExpr <- compileExpr e
     tmpSym <- generateTmpSym
     prereq $ return $ SH.Sequence
       (noAnnotation $ SH.Assign $ SH.Var (asId tmpSym) cExpr) (noAnnotation SH.Empty)
     return $ "$" <> asIdString tmpSym

doubleTempVar :: Expression -> ExpressionCompilerMonad AssignedSymbol
doubleTempVar expr = do
  numExpr <- compileDoubleExpr expr
  tmpSym <- generateTmpSym
  prereq $ return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym) <> "=$(bc -l <<< \"" <> numExpr <> "\")"] []) SH.Empty) (noAnnotation SH.Empty)
  return tmpSym

boolSubExpr :: Expression -> ExpressionCompilerMonad (Maybe BashExpression)
boolSubExpr expr =
    case expr of
      EUnaryOp UONot _ -> boolSubExpr'
      EBinOp BOAnd _ _ -> boolSubExpr'
      EBinOp BOOr _ _ -> boolSubExpr'
      EBinOp BOEq _ _ -> boolSubExpr'
      EBinOp BONeq _ _ -> boolSubExpr'
      _ -> return Nothing
  where
    boolSubExpr' = do
      tmpSym <- boolTempVar expr
      return $ Just (SH.ReadVar (SH.VarIdent $ asId tmpSym))

numericSubExpr :: Expression -> ExpressionCompilerMonad (Maybe BashExpression)
numericSubExpr expr =
    case expr of
      EBinOp BOAdd a b -> numericSubExpr' a b
      EBinOp BOSub a b -> numericSubExpr' a b
      EBinOp BOMul a b -> numericSubExpr' a b
      EBinOp BODiv a b -> numericSubExpr' a b
      EBinOp BOMod a b -> numericSubExpr' a b
      EIntLit n -> return $ Just (SH.literal (B.fromString $ show n))
      EDoubleLit n -> return $ Just (SH.literal (B.fromString $ show n))
      _ -> return Nothing
  where
    numericSubExpr' :: Expression -> Expression -> ExpressionCompilerMonad (Maybe BashExpression)
    numericSubExpr' a b = do
      (!ta) <- typeCheckExpr a
      (!tb) <- typeCheckExpr b
      case (ta, tb) of
       (SimpleType TString, SimpleType TString) -> return Nothing
       (SimpleType TDouble, _) -> subExpr doubleTempVar
       (_, SimpleType TDouble) -> subExpr doubleTempVar
       _ -> subExpr integerTempVar

    subExpr fn = do
      tmpSym <- fn expr
      return $ Just (SH.ReadVar (SH.VarIdent $ asId tmpSym))

stringSubExpr :: Expression -> ExpressionCompilerMonad (Maybe BashExpression)
stringSubExpr expr =
  case expr of
    EBinOp BOAdd e1 e2 -> do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2

      case (t1, t2) of
        (SimpleType TString, SimpleType TString) -> do
          c1 <- trace ("String concatenation of " <> (show e1) <> " and " <> (show e2)) $ compileExpr e1
          c2 <- compileExpr e2
          return $ Just $ SH.Concat c1 c2
        _ -> return Nothing
    _ -> return Nothing

compileExpr :: Expression -> ExpressionCompilerMonad BashExpression
compileExpr expr =
    trace ("Compiling " <> show expr) $ case expr of
      EStringLit str -> return ((SH.literal . B.fromString) str)

      EBoolLit False -> return $ SH.literal "1"
      EBoolLit True -> return $ SH.literal "0"

      EVar sym -> do
                r <- findSymbolM sym
                t <- findTypeM sym
                case r of
                  Just asym ->
                      case t of
                        Just (SimpleType (TFun{})) -> return (SH.literal $ asIdString asym)
                        _ ->  return (SH.ReadVar (SH.VarIdent $ asId asym))
                  Nothing -> throwError $ UndefinedSymbol sym

      EArrayAccess sym indexExpr -> do
        asym <- findSymbolM sym
        case asym of
          Just arraySym -> do
            cIdx <- compileExpr indexExpr
            tmpSym <- generateTmpSym
            prereq $ return $ SH.Sequence
              (SH.Annotated (SH.Lines ["declare -a " <> (asIdString tmpSym) <> "=(${" <> asIdString arraySym <> "[*]})"] []) SH.Empty) (noAnnotation SH.Empty)
            return $ SH.ReadArray (asId tmpSym) cIdx
          Nothing ->
            throwError $ UndefinedSymbol sym

      EPredefined sym ->
        case Map.lookup sym predefined of
          Just (PredefinedValue{..}) ->
            case predefValue of
              PredefinedExpression e -> compileExpr e
              CustomExpression _ -> throwError $ InvalidUseOfPredefinedFunction sym
          Nothing -> throwError $ UndefinedSymbol sym

      ESysVar sym -> return $ SH.ReadVar (SH.VarIdent ((SH.Identifier . B.fromString) sym))

      EApply (EPredefined sym) params | isCustomPredefinedExpression sym ->
        case predefValue (predefined Map.! sym) of
          CustomExpression fn -> do
            paramTypes <- mapM typeCheckExpr params
            fn (\e c -> compileExpression' e compileExpr c) $ zipWith TypedExpression paramTypes params
          _ -> throwError $ GeneralError "Impossible state (isCustomPredefinedExpression failure)"

      EApply funRefExpr params -> do
                funRef <- compileExpr funRefExpr
                cParams <- mapM compileExpr params
                tmpSym <- generateTmpSym
                prereq $ return $ SH.Sequence
                  (noAnnotation $ SH.Assign $ SH.Var (asId tmpSym) (SH.literal ""))
                  (noAnnotation $ SH.SimpleCommand funRef $ (SH.literal (asIdString tmpSym)):cParams)
                return $ SH.ReadVar (SH.VarIdent $ asId tmpSym)

      _ -> do
        sr <- stringSubExpr expr
        br <- boolSubExpr expr
        nr <- numericSubExpr expr
        case sr <|> br <|> nr of
         Just result -> return result
         Nothing -> throwError $ NotSupported (show expr)

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
                       compileExpression' expr compileExpr $
                         \cExpr -> return $ SH.Assign $ SH.Var (asId asym) cExpr

      SArrayDecl sym _ -> do
        asym <- createIdentifierM sym
        return $ SH.Declare $ SH.Array (asId asym) []

      SDefFun sym _props _tps pdef rettype stIn -> do
              r <- findSymbolM sym
              case r of
                Just _ -> throwError $ SymbolAlreadyDefined sym
                Nothing -> do
                       asym <- createIdentifierM sym
                       -- TODO: add type params to context
                       funCtx <- funContextM sym pdef
                       cStIn <- runChildContext funCtx $ (typeCheckSt stIn >> compileSt stIn)
                       return $ SH.Function (SH.Simple $ asId asym) (noAnnotation $ withFunctionHeader funCtx pdef rettype cStIn)

      SSequence s1 s2 -> do
              t1 <- compileSt s1
              t2 <- compileSt s2
              return $ SH.Sequence (noAnnotation t1) (noAnnotation t2)

      SCall funRefExpr params ->
        compileExpression' funRefExpr compileExpr $ \funRef ->
          compileExpressions' params compileExpr $ \cParams -> return $ SH.SimpleCommand funRef cParams

      SReturn expr ->
        compileExpression' expr compileExpr $ \cExpr -> do
          ctx <- get
          exprType <- typeCheckExpr expr
          case exprType of
            SimpleType (TArray _) -> do
              case expr of
                EVar sym -> do
                  asym <- findSymbolM sym
                  case asym of
                    Just s -> do
                      let passToCaller = B.fromString $ printf "eval $%s=\\\"${%s[*]}\\\"" (retVarName ctx) (B.toString $ asIdString s)
                      return $ SH.Sequence (SH.Annotated (SH.Lines [] [passToCaller]) SH.Empty) (noAnnotation SH.Empty)
                    Nothing ->
                      throwError $ UndefinedSymbol sym
                _ -> throwError $ NotSupported $ "Returning " <> show expr
            _ -> do
              let setRetVal = SH.Local (SH.Var (retValId ctx) cExpr)
                  passToCaller = B.fromString $ printf "eval $%s=\"$%s\"" (retVarName ctx) (retValName ctx)
              return $ SH.Sequence (SH.Annotated (SH.Lines [] [passToCaller]) setRetVal) (noAnnotation SH.Empty)

      SRun program params ->
        compileExpression' program compileExpr $ \cProgram ->
          compileExpressions' params compileExpr $ \cParams -> return $ SH.SimpleCommand cProgram cParams

      SIf cond tbody fbody -> do
        (condExpr, prereqs) <- runWriterT (compileBoolExpr cond)
        tContext <- cloneContext
        fContext <- cloneContext
        cTBody <- runChildContext tContext (typeCheckSt tbody >> compileSt tbody)
        cFBody <- runChildContext fContext (typeCheckSt fbody >> compileSt fbody)
        let result = SH.IfThenElse (SH.Annotated (SH.Lines [condExpr] []) SH.Empty) (noAnnotation cTBody) (noAnnotation cFBody)
        prereqs <> return result

      SWhile cond body -> do
        (condExpr, prereqs) <- runWriterT (compileBoolExpr cond)
        childContext <- cloneContext
        cBody <- runChildContext childContext (typeCheckSt body >> compileSt body)
        let result = SH.While (SH.Annotated (SH.Lines [condExpr] []) SH.Empty) (noAnnotation cBody)
        prereqs <> return result

      SUpdateCell sym iExpr expr -> do
        r <- findSymbolM sym
        case r of
          Just asym -> do
            compileExpression' iExpr compileExpr $
              \ciExpr -> compileExpression' expr compileExpr $
                \cExpr -> return $ SH.ArrayUpdate (asId asym) ciExpr cExpr
          Nothing -> do
            dumpState
            throwError $ UndefinedSymbol sym

      SUpdateVar sym expr -> do
        r <- findSymbolM sym
        case r of
          Just asym -> do
            compileExpression' expr compileExpr $
              \cExpr -> return $ SH.Assign $ SH.Var (asId asym) cExpr
          Nothing ->
            throwError $ UndefinedSymbol sym

unifyTypeVars :: (MonadState Context m, MonadError CompilerError m) => [TypeParam] -> [ExtendedType] -> [Type] -> m (Map.Map SymbolName Type)
unifyTypeVars typeParams actualTypes typeDefs = do
  let typeVarNames = Set.fromList $ map (\(TypeParam n) -> n) typeParams
  let pairs = zip actualTypes typeDefs
  let mappings = mapMaybe (findTypeMapping typeVarNames) pairs
  -- TODO: error on ambiguous mapping
  return $ Map.fromList mappings

  where
    findTypeMapping typeVarNames ((SimpleType actual), (TVar n)) | n `Set.member` typeVarNames =
      Just (n, actual)
    findTypeMapping _ _ =
      Nothing

appliedType :: Map.Map SymbolName Type -> Type -> Type
appliedType mapping (TVar n) | n `Map.member` mapping =
  mapping Map.! n
appliedType mapping (TFun tps ps r) =
  TFun tps (map (appliedType mapping) ps) (appliedType mapping r)
appliedType _ t = t

appliedExtType :: Map.Map SymbolName Type -> Type -> ExtendedType
appliedExtType mapping t = SimpleType $ appliedType mapping t

typeCheckExpr :: (MonadState Context m, MonadError CompilerError m) => Expression -> m ExtendedType
typeCheckExpr expr =
    case expr of
      EStringLit _ -> return $ SimpleType TString

      EBoolLit _ -> return $ SimpleType TBool

      EIntLit _ -> return $ SimpleType TInt

      EDoubleLit _ -> return $ SimpleType TDouble

      EVar sym -> do
        t <- findTypeM sym
        case t of
          Just typ ->
            return typ
          Nothing -> do
            throwError $ CannotInferType sym

      EPredefined sym ->
        case Map.lookup sym predefined of
          Just (PredefinedValue{..}) -> return $ SimpleType predefType
          Nothing -> throwError $ CannotInferType sym

      ESysVar _ -> return $ SimpleType TString

      EArrayAccess sym indexExpr -> do
        arrayType <- findTypeM sym
        idxType <- typeCheckExpr indexExpr
        case arrayType of
          Just (SimpleType (TArray elemType)) ->
            case idxType of
              SimpleType TInt ->
                return $ SimpleType elemType
              _ ->
                throwError $ InvalidArrayIndexType idxType
          Just t ->
            throwError $ NonArrayTypeIndexed t
          Nothing ->
            throwError $ UndefinedSymbol sym

      ELambda tps params rett _body ->
        -- TODO: typecheck body
        return $ SimpleType $ TFun tps (map (\(ParamDef (_, t)) -> t) params) rett

      EApply funRefExpr params -> do
          let funName = trace ("funRefExpr: " <> show funRefExpr <> "; params: " <> show params) $ show funRefExpr
          paramTypes <- mapM typeCheckExpr params
          fnType <- typeCheckExpr funRefExpr
          case fnType of
            SimpleType (TFun tps expectedTypes retType) -> do
              typeVarMapping <- unifyTypeVars tps paramTypes expectedTypes
              case (map (appliedExtType typeVarMapping) expectedTypes) `typeEq` paramTypes of
                True -> return $ appliedExtType typeVarMapping retType
                False -> throwError $ InvalidParameterTypes funName expectedTypes paramTypes
            TFunRef tps expectedTypes retType -> do
              typeVarMapping <- unifyTypeVars tps paramTypes expectedTypes
              case (map (appliedExtType typeVarMapping) expectedTypes) `typeEq` paramTypes of
                True -> return $ appliedExtType typeVarMapping retType
                False -> throwError $ InvalidParameterTypes funName expectedTypes paramTypes
            _ -> throwError $ SymbolNotBoundToFunction funName

      EUnaryOp UONot e -> do
        t <- typeCheckExpr e
        case t of
         SimpleType TBool -> return $ SimpleType TBool
         _ -> throwError $ InvalidBooleanExpression $ Just expr

      EBinOp BOAnd e1 e2 -> typeCheckBinaryBoolExpr e1 e2
      EBinOp BOOr e1 e2 -> typeCheckBinaryBoolExpr e1 e2

      EBinOp BOAdd e1 e2 -> do
        t1 <- typeCheckExpr e1
        t2 <- typeCheckExpr e2
        case (t1, t2) of
          (SimpleType TString, SimpleType TString) -> return $ SimpleType TString
          _ -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EBinOp BOSub e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EBinOp BOMul e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EBinOp BODiv e1 e2 -> typeCheckBinaryNumericExpr e1 e2 Nothing
      EBinOp BOMod e1 e2 -> do
        t <- typeCheckBinaryNumericExpr e1 e2 Nothing
        case t of
          SimpleType TInt -> return $ SimpleType TInt
          _  -> throwError $ InvalidTypesInNumericExpression [t]

      EBinOp BOLess e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      EBinOp BOLessEq e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      EBinOp BOGreater e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool
      EBinOp BOGreaterEq e1 e2 -> typeCheckBinaryNumericExpr e1 e2 $ Just TBool

      EBinOp BOEq e1 e2 -> typeCheckEqualityExpr e1 e2
      EBinOp BONeq e1 e2 -> typeCheckEqualityExpr e1 e2

  where
    typeCheckBinaryBoolExpr e1 e2 = do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2
      case (t1, t2) of
       (SimpleType TBool, SimpleType TBool) -> return $ SimpleType TBool
       _ -> throwError $ InvalidBooleanExpression Nothing

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

funType :: [TypeParam] -> [ParamDef] -> Type -> Type
funType tps params rettype = TFun tps (map (\case { ParamDef (_, t) -> t }) params) rettype

funToFunRef :: Type -> ExtendedType
funToFunRef (TFun tps ps t) = TFunRef tps ps t
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

      SArrayDecl sym elemType -> do
        storeTypeM sym $ SimpleType $ TArray elemType
        return $ SimpleType TUnit

      SDefFun sym _props tps pdef rettype stIn -> do
        storeTypeM sym (SimpleType $ funType tps pdef rettype)
        -- TODO: add type params to context
        funCtx <- funContextM sym pdef
        (!sttype) <- runChildContext funCtx $ typeCheckSt stIn
        if sttype `typeEq` SimpleType rettype
          then return $ SimpleType TUnit
          else throwError $ InvalidReturnType sym rettype sttype

      SSequence s1 s2 -> do
        !_ <- typeCheckSt s1
        !r <- typeCheckSt s2
        return r

      SCall funNameExpr params -> do
        (!typ) <- typeCheckExpr $ EApply funNameExpr params
        return typ

      SReturn expr -> do
        (!typ) <- typeCheckExpr expr
        return typ

      SRun program params -> do
        (!typProgram) <- typeCheckExpr program
        typParams <- mapM typeCheckExpr params
        if (typProgram `typeEq` SimpleType TString) && all (\t -> t `typeEq` SimpleType TString) typParams
          then return $ SimpleType TUnit -- TODO
          else (trace $ "program: " <> show program <> " params: " <> show params <> " with types " <> show typParams) $ throwError InvalidParametersForRunStatement

      SIf cond tbody fbody -> do
        (!condTyp) <- typeCheckExpr cond
        if condTyp `typeEq` SimpleType TBool
          then do
            (!_) <- typeCheckSt tbody
            (!_) <- typeCheckSt fbody
            return $ SimpleType TUnit
          else
            throwError $ InvalidConditionalExpressionTypeForIf condTyp

      SWhile cond body -> do
        (!condTyp) <- typeCheckExpr cond
        if condTyp `typeEq` SimpleType TBool
        then do
          childContext <- cloneContext
          (!_) <- runChildContext childContext $ typeCheckSt body
          return $ SimpleType TUnit
        else
          throwError $ InvalidConditionalExpressionTypeForWhile condTyp

      SUpdateVar sym expr -> do
        (!typ) <- typeCheckExpr expr
        existingTyp <- findTypeM sym
        case existingTyp of
          Just t | typ `typeEq` t ->
            return $ SimpleType TUnit
          Just t ->
            throwError $ VariableUpdateTypeMismatch typ t
          Nothing ->
            throwError $ UndefinedSymbol sym


      SUpdateCell sym iExpr expr -> do
        !iTyp <- typeCheckExpr iExpr
        !typ <- typeCheckExpr expr
        existingTyp <- findTypeM sym
        case existingTyp of
          Just (SimpleType (TArray t)) | typ `typeEq` (SimpleType t) ->
            case iTyp of
              SimpleType TInt -> return $ SimpleType TUnit
              _ -> throwError $ InvalidArrayIndexType iTyp
          Just t ->
            throwError $ VariableUpdateTypeMismatch typ t
          Nothing ->
            throwError $ UndefinedSymbol sym
