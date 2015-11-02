{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Optimization phase (most often for readability)
module Language.Barlang.Optimizer where

import           Control.Monad.State
import qualified Data.Map.Lazy             as Map
import           Data.Monoid
import           Debug.Trace
import           Language.Barlang.Language

data DefinedFunction =
  DefinedFunction
  { dfName       :: SymbolName
  , dfTypeParams :: [TypeParam]
  , dfParams     :: [ParamDef]
  , dfReturnType :: Type
  , dfBody       :: Statement
  , dfAutoInline :: Bool
  }
  deriving (Show, Eq)

data OptimizerState =
  OptimizerState
  { osFunctionMap :: Map.Map SymbolName DefinedFunction
  }
  deriving (Show, Eq)

type OptimizerStateMonad a = State OptimizerState a

initialState :: OptimizerState
initialState =
  OptimizerState
  { osFunctionMap = Map.empty
  }

isReturn :: SingleStatement -> Bool
isReturn (SSReturn _) = True
isReturn _ = False

markAutoInlineable :: DefinedFunction -> DefinedFunction
markAutoInlineable def@DefinedFunction{..} =
  let statements = normalize dfBody
      inlineable = length statements == 1 && not (isReturn (head statements))
  in def { dfAutoInline = inlineable }

defineFunction :: Statement -> OptimizerStateMonad ()
defineFunction (SDefFun name props tps ps rett body) =
  modify (\state ->
            state { osFunctionMap = Map.insert name def (osFunctionMap state) })

  where
    def = markAutoInlineable DefinedFunction
          { dfName = name
          , dfTypeParams = tps
          , dfParams = ps
          , dfReturnType = rett
          , dfBody = body
          , dfAutoInline = False
          }

replaceSubExpression :: Expression -> Expression -> Expression -> Expression
replaceSubExpression from to x =
  if x == from
  then to
  else case x of
    EApply e0 es -> EApply (replaceSubExpression from to e0) (map (replaceSubExpression from to) es)
    EAnd e1 e2 -> EAnd (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EOr e1 e2 -> EOr (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    ENot e1 -> ENot (replaceSubExpression from to e1)
    EAdd e1 e2 -> EAdd (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    ESub e1 e2 -> ESub (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EMul e1 e2 -> EMul (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EDiv e1 e2 -> EDiv (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EEq e1 e2 -> EEq (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    ENeq e1 e2 -> ENeq (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    ELess e1 e2 -> ELess (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    ELessEq e1 e2 -> ELessEq (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EGreater e1 e2 -> EGreater (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    EGreaterEq e1 e2 -> EGreaterEq (replaceSubExpression from to e1) (replaceSubExpression from to e2)
    e -> e


replaceExpression :: Expression -> Expression -> Statement -> Statement
replaceExpression from to = \case
  SVarDecl n x -> SVarDecl n $ replaceSubExpression from to x
  SSequence s1 s2 -> SSequence (replaceExpression from to s1) (replaceExpression from to s2)
  SCall nameExpr paramExprs -> SCall (replaceSubExpression from to nameExpr) (map (replaceSubExpression from to) paramExprs)
  SRun nameExpr paramExprs -> SRun (replaceSubExpression from to nameExpr) (map (replaceSubExpression from to) paramExprs)
  SReturn x -> SReturn $ replaceSubExpression from to x
  s -> s

inlinedFunctionApplication :: DefinedFunction -> [Expression] -> Statement
inlinedFunctionApplication DefinedFunction{..} paramExprs =
  foldl (\s (ParamDef (pname, _), pexpr) -> replaceExpression (EVar pname) pexpr s) dfBody (dfParams `zip` paramExprs)

optimizeStatement :: Statement -> OptimizerStateMonad Statement
optimizeStatement = \case
  SSequence s1 s2 -> SSequence <$> optimizeStatement s1 <*> optimizeStatement s2

  SDefFun name props tps ps rett body -> do
    optimizedBody <- optimizeStatement body
    let optimizedFun = SDefFun name props tps ps rett optimizedBody
    defineFunction optimizedFun
    return optimizedFun

  s@(SCall (EVar name) paramExprs) -> get >>= \state -> do
    case Map.lookup name (osFunctionMap state) of
      Just def ->
        if dfAutoInline def
        then trace ("Inlineing call to " <> name) $ pure $ inlinedFunctionApplication def paramExprs
        else pure s
      Nothing -> pure s

  s -> pure s

optimize :: Script -> Script
optimize s@Script{..} = trace "optimizing.." $ -- TODO: remove trace, use proper logging
  s { sStatement = evalState (optimizeStatement sStatement) initialState }
