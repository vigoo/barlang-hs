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
import qualified Language.Bash.Annotations      as SH
import qualified Language.Bash.Syntax           as SH

data PredefinedExpression
  = PredefinedExpression Expression
  | CustomExpression ([TypedExpression] -> ExpressionCompilerMonad BashExpression)

data PredefinedValue
  = PredefinedValue
    { predefName  :: SymbolName
    , predefType  :: Type
    , predefValue :: PredefinedExpression
    }

isCustomPredefinedExpression :: SymbolName -> Bool
isCustomPredefinedExpression name =
  case (Map.lookup name predefined) of
    Just (PredefinedValue{..}) ->
      case predefValue of
        CustomExpression _ -> True
        _ -> False
    Nothing -> False

predef :: SymbolName -> Type -> Expression -> (SymbolName, PredefinedValue)
predef n t e = (n, PredefinedValue { predefName = n, predefType = t, predefValue = PredefinedExpression e})

custom :: SymbolName -> Type -> ([TypedExpression] -> ExpressionCompilerMonad BashExpression) -> (SymbolName, PredefinedValue)
custom n t f = (n, PredefinedValue { predefName = n, predefType = t, predefValue = CustomExpression f})

predefined :: Map.Map SymbolName PredefinedValue
predefined = Map.fromList $
  [ predef "pi" TDouble (EDoubleLit pi)
  , custom "str" (TFun [TypeParam "T"] [TVar "T"] TString) str
  ]

str :: [TypedExpression] -> ExpressionCompilerMonad BashExpression
str [param] = case param of
  (TypedExpression (SimpleType TString) (EStringLit str)) -> litString str
  (TypedExpression (SimpleType TBool) (EBoolLit True)) -> litString "true"
  (TypedExpression (SimpleType TBool) (EBoolLit False)) -> litString "false"
  (TypedExpression (SimpleType TInt) (EIntLit v)) -> litString (show v)
  (TypedExpression (SimpleType TDouble) (EDoubleLit d)) -> litString (show d)
  (TypedExpression typ (EVar sym)) -> do
    r <- findSymbolM sym
    case r of
      Just asym ->
        case typ of
          (SimpleType TString) -> return (SH.ReadVar (SH.VarIdent $ asId asym))
          (SimpleType TBool) -> return (SH.ReadVar (SH.VarIdent $ asId asym)) -- TODO: custom formatting
          (SimpleType TInt) -> return (SH.ReadVar (SH.VarIdent $ asId asym))
          (SimpleType TDouble) -> return (SH.ReadVar (SH.VarIdent $ asId asym)) -- TODO: custom formatting
          _ -> throwError $ GeneralError $ "Unsupported variable type in 'str: " <> show typ -- TODO: better error message
      Nothing -> throwError $ UndefinedSymbol sym
  -- TODO: support evaluating subexpressions
  _ -> throwError $ GeneralError "Unsupported parameter for 'str'" -- TODO: better error message

  where
    litString s = return ((SH.literal . B.fromString) s)

str _ = throwError $ GeneralError "Invalid parameters for 'str'" -- TODO: better error message
