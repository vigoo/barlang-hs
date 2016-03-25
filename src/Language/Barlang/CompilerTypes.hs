{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Barlang.CompilerTypes where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer.Lazy
import qualified Data.ByteString.UTF8      as B
import qualified Data.Map                  as Map
import           Debug.Trace
import           Language.Barlang.Language
import qualified Language.Bash             as SH
import qualified Language.Bash.Syntax      as SH
import           Text.Printf

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
                   | InvalidBooleanExpression (Maybe Expression)
                   | InvalidTypesInNumericExpression [ExtendedType]
                   | EqualityUsedOnNonEqualTypes [ExtendedType]
                   | UnsupportedTypeInBooleanExpression ExtendedType
                   | InvalidUseOfPredefinedFunction SymbolName
                   | InvalidConditionalExpressionTypeForIf ExtendedType
                   | InvalidConditionalExpressionTypeForWhile ExtendedType
                   | InvalidParameterTypeForPredefined SymbolName [ExtendedType]
                   | VariableUpdateTypeMismatch ExtendedType ExtendedType
                   | GeneralError String
                     deriving (Show)

type CompilerMonad = ExceptT CompilerError (State Context)
type ExpressionCompilerMonad = WriterT (CompilerMonad BashStatement) CompilerMonad

data AssignedSymbol = AssignedSymbol { asName     :: SymbolName
                                     , asIdString :: B.ByteString
                                     , asId       :: SH.Identifier
                                     }
                    deriving (Show)

data ExtendedType = SimpleType Type
                  | TFunRef [TypeParam] [Type] Type
                  deriving (Show)

data TypedExpression =
  TypedExpression { texpType :: ExtendedType
                  , texpExpr :: Expression
                  }

data Context = Context { ctxScope       :: Scope
                       , ctxSymbols     :: Map.Map SymbolName AssignedSymbol
                       , ctxSymbolTypes :: Map.Map SymbolName ExtendedType
                       , ctxLastTmp     :: Int
                       }
               deriving (Show)


prereq :: CompilerMonad BashStatement -> ExpressionCompilerMonad ()
prereq = tell

noPrereq :: CompilerMonad BashStatement
noPrereq = return SH.Empty

mergeStatements :: CompilerMonad BashStatement -> CompilerMonad BashStatement -> CompilerMonad BashStatement
mergeStatements ma mb = do
  a <- ma
  b <- mb
  return $ SH.Sequence (noAnnotation a) (noAnnotation b)

noAnnotation :: BashStatement -> SH.Annotated SH.Lines
noAnnotation = SH.Annotated (SH.Lines [] [])

instance Monoid (CompilerMonad BashStatement) where
  mappend = mergeStatements
  mempty = noPrereq


storeType :: Context -> SymbolName -> ExtendedType -> Context
storeType ctx@Context{..} sym typ = ctx { ctxSymbolTypes = Map.insert sym typ ctxSymbolTypes }

storeTypeM :: (MonadState Context m) => SymbolName -> ExtendedType -> m ()
storeTypeM sym typ = modify (\ctx -> storeType ctx sym typ)

findSymbol :: Context -> SymbolName -> Maybe AssignedSymbol
findSymbol Context{..} sym = Map.lookup sym ctxSymbols

findSymbolM :: (MonadState Context m) => SymbolName -> m (Maybe AssignedSymbol)
findSymbolM sym = gets $ \ctx -> findSymbol ctx sym

findType :: Context -> SymbolName -> Maybe ExtendedType
findType Context{..} sym = Map.lookup sym ctxSymbolTypes

findTypeM :: (MonadState Context m) => SymbolName -> m (Maybe ExtendedType)
findTypeM sym = gets $ \ctx -> findType ctx sym

generateTmpSym :: (MonadState Context m) => m AssignedSymbol
generateTmpSym = do
  ctx <- get
  let next = 1 + ctxLastTmp ctx
      idString = B.fromString $ printf "%s_tmp%d" (ctxScope ctx) next
  put $ ctx { ctxLastTmp = next }
  return $ AssignedSymbol (printf "tmp%d" next) idString $ SH.Identifier idString

cloneContext :: (MonadState Context m) => m Context
cloneContext = get

runChildContext :: Context -> CompilerMonad a -> CompilerMonad a
runChildContext ctx' f = let res = evalState (runExceptT f) ctx'
                         in case res of
                              Right v -> return v
                              Left err -> throwError err


dumpState :: (MonadState Context m) => m ()
dumpState = do
  ctx <- get
  traceShow ctx $ return ()
