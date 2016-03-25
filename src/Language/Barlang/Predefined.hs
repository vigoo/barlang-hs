{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Barlang.Predefined
  ( predefined
  , PredefinedValue(..)
  , PredefinedExpression(..)
  , isCustomPredefinedExpression
  )
where

import           Control.Monad.Except
import qualified Data.ByteString.UTF8           as B
import qualified Data.Map                       as Map
import           Data.Monoid
import           Language.Barlang.CompilerTypes
import           Language.Barlang.Language
import qualified Language.Bash                  as SH

type CompileExpressionFn = Expression -> (BashExpression -> CompilerMonad BashStatement) -> CompilerMonad BashStatement

data PredefinedExpression
  = PredefinedExpression Expression
  | CustomExpression (CompileExpressionFn -> [TypedExpression] -> ExpressionCompilerMonad BashExpression)

data PredefinedValue
  = PredefinedValue
    { predefName  :: SymbolName
    , predefType  :: Type
    , predefValue :: PredefinedExpression
    }

isCustomPredefinedExpression :: SymbolName -> Bool
isCustomPredefinedExpression name =
  case Map.lookup name predefined of
    Just (PredefinedValue{..}) ->
      case predefValue of
        CustomExpression _ -> True
        _ -> False
    Nothing -> False

predef :: SymbolName -> Type -> Expression -> (SymbolName, PredefinedValue)
predef n t e = (n, PredefinedValue { predefName = n, predefType = t, predefValue = PredefinedExpression e})

custom :: SymbolName -> Type -> (CompileExpressionFn -> [TypedExpression] -> ExpressionCompilerMonad BashExpression) -> (SymbolName, PredefinedValue)
custom n t f = (n, PredefinedValue { predefName = n, predefType = t, predefValue = CustomExpression f})

predefined :: Map.Map SymbolName PredefinedValue
predefined = Map.fromList
  [ predef "pi" TDouble (EDoubleLit pi)
  , custom "str" (TFun [TypeParam "T"] [TVar "T"] TString) str
  , custom "sin" (TFun [] [TDouble] TDouble) (bcfun "sin" "s")
  , custom "cos" (TFun [] [TDouble] TDouble) (bcfun "cos" "c")
  , custom "toInt" (TFun [] [TDouble] TInt) int
  ]

int :: CompileExpressionFn -> [TypedExpression] -> ExpressionCompilerMonad BashExpression
int compileExpression [TypedExpression (SimpleType TDouble) expr] = do
  tmpSym <- generateTmpSym
  tmpSym' <- generateTmpSym
  prereq $ compileExpression expr $ \cExpr ->
    return $ SH.Assign $ SH.Var (asId tmpSym) cExpr
  prereq $
    return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym') <> "=$(printf '%.0f' $" <> (asIdString tmpSym) <> ")"] []) SH.Empty) (noAnnotation SH.Empty)
  return (SH.ReadVar (SH.VarIdent $ asId tmpSym'))

int _ ps = throwError $ InvalidParameterTypeForPredefined "int" (map texpType ps)

bcfun :: String -> String -> CompileExpressionFn -> [TypedExpression] -> ExpressionCompilerMonad BashExpression
bcfun _ fun compileExpression [TypedExpression (SimpleType TDouble) expr] = do
  tmpSym <- generateTmpSym
  prereq $ compileExpression expr $
    \cExpr -> return $ SH.Assign $ SH.Var (asId tmpSym) cExpr

  tmpSym' <- generateTmpSym
  prereq $ return $ SH.Sequence (SH.Annotated (SH.Lines [(asIdString tmpSym') <> "=$(bc -l <<< \"" <> B.fromString fun <> "($" <> (asIdString tmpSym) <> ")\")"] []) SH.Empty) (noAnnotation SH.Empty)
  return (SH.ReadVar (SH.VarIdent $ asId tmpSym'))

bcfun n _ _ ps = throwError $ InvalidParameterTypeForPredefined n (map texpType ps)

str :: CompileExpressionFn -> [TypedExpression] -> ExpressionCompilerMonad BashExpression
str compileExpression [param] = case param of
  (TypedExpression (SimpleType TString) (EStringLit s)) -> litString s
  (TypedExpression (SimpleType TBool) (EBoolLit True)) -> litString "true"
  (TypedExpression (SimpleType TBool) (EBoolLit False)) -> litString "false"
  (TypedExpression (SimpleType TInt) (EIntLit v)) -> litString (show v)
  (TypedExpression (SimpleType TDouble) (EDoubleLit d)) -> litString (show d)
  (TypedExpression typ (EVar sym)) -> do
    r <- findSymbolM sym
    case r of
      Just asym -> strVar typ asym
      Nothing -> throwError $ UndefinedSymbol sym
  (TypedExpression typ expr) -> do
    tmpSym <- generateTmpSym
    prereq $ compileExpression expr $
               \cExpr -> return $ SH.Assign $ SH.Var (asId tmpSym) cExpr
    strVar typ tmpSym

  where
    litString' = SH.literal . B.fromString
    litString s = return $ litString' s

    strVar typ asym =
      case typ of
        (SimpleType TString) -> return (SH.ReadVar (SH.VarIdent $ asId asym))
        (SimpleType TBool) -> boolToString asym
        (SimpleType TInt) -> return (SH.ReadVar (SH.VarIdent $ asId asym))
        (SimpleType TDouble) -> return (SH.ReadVar (SH.VarIdent $ asId asym))
        _ -> throwError $ InvalidParameterTypeForPredefined "str" [typ]

    boolToString asym = do
      tmpSym <- generateTmpSym
      let condExpr = "[[ ( \"$" <> asIdString asym <> "\" = \"0\" ) ]]"
      let onTrue = SH.Assign $ SH.Var (asId tmpSym) $ litString' "true"
      let onFalse = SH.Assign $ SH.Var (asId tmpSym) $ litString' "false"
      prereq $ return $ SH.IfThenElse (SH.Annotated (SH.Lines [condExpr] []) SH.Empty) (noAnnotation onTrue) (noAnnotation onFalse)
      return (SH.ReadVar (SH.VarIdent $ asId tmpSym))

str _ ps = throwError $ InvalidParameterTypeForPredefined "str" (map texpType ps)
