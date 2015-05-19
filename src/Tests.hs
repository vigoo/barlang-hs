import Control.Applicative
import Control.Monad
import Data.Char
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Text.Trifecta.Result as Tr

import Language.Barlang.Language
import Language.Barlang.Parser
import Language.Barlang.PrettyPrint

-- Arbitrary instances
arbitrarySymbolName :: Gen SymbolName
arbitrarySymbolName = listOf1 (arbitrary `suchThat` (\c -> isAscii c && isAlpha c))

arbitraryStringLiteral :: Gen SymbolName
arbitraryStringLiteral = listOf $ arbitrary `suchThat` (\c -> isAscii c && (isAlpha c || c == ' ')) -- TODO

instance Arbitrary Type where
  arbitrary = sized arbType
   where
     arbType :: Int -> Gen Type
     arbType 0 = oneof [ pure TUnit
                       , pure TString
                       , pure TBool
                       , pure TInt
                       , pure TDouble
                       ]
     arbType n = oneof [ pure TUnit
                       , pure TString
                       , pure TBool
                       , pure TInt
                       , pure TDouble
                       , do k <- choose (0, n-1)
                            l <- choose (0, n-1)
                            m <- choose (0, n-1)
                            args <- replicateM k $ arbType l
                            rettype <- arbType m
                            return $ TFun args rettype
                       ]

  shrink (TFun args rettype) = [TFun args' rettype| args' <- shrink args] ++
                               [TFun args rettype'| rettype' <- shrink rettype]
  shrink _ = []

instance Arbitrary Expression where
  arbitrary = sized arbExpr
    where
      arbExpr :: Int -> Gen Expression
      arbExpr 0 = oneof [ EStringLit <$> arbitraryStringLiteral
                        , EBoolLit <$> arbitrary
                        , EIntLit <$> arbitrary
                        , EDoubleLit <$> arbitrary
                        , EVar <$> arbitrarySymbolName
                        , ESysVar <$> arbitrarySymbolName
                        ]
      arbExpr n = oneof [ EStringLit <$> arbitraryStringLiteral
                        , EBoolLit <$> arbitrary
                        , EIntLit <$> arbitrary
                        , EDoubleLit <$> arbitrary
                        , EVar <$> arbitrarySymbolName
                        , ESysVar <$> arbitrarySymbolName
                        , do k <- choose (0, n-1)
                             l <- choose (0, n-1)
                             args <- replicateM k $ arbExpr l
                             fn <- EVar <$> arbitrarySymbolName
                             return $ EApply fn args
                        , binary EAnd
                        , binary EOr
                        , binary EAdd
                        , binary ESub
                        , binary EMul
                        , binary EDiv
                        , binary EEq
                        , binary ENeq
                        , binary ELess
                        , binary ELessEq
                        , binary EGreater
                        , binary EGreaterEq
                        , unary ENot
                        ]
            where
              unary c = do
                l1 <- choose (0, n-1)
                e1 <- arbExpr l1
                return $ c e1
              binary c = do
                l1 <- choose (0, n-1)
                l2 <- choose (0, n-1)
                e1 <- arbExpr l1
                e2 <- arbExpr l2
                return $ c e1 e2

  shrink (EStringLit _) = [EStringLit "hello"]
  shrink (EIntLit _) = [EIntLit 1]
  shrink (EDoubleLit _) = [EDoubleLit 0.1]
  shrink (EVar n) | (length n > 1) = [EVar "x"]
                  | otherwise = []
  shrink (ESysVar n) | (length n > 1) = [ESysVar "V"]
                  | otherwise = []
  shrink (EApply sym args) = [EApply sym' args' | sym' <- shrink sym, args' <- shrink args]
  shrink (EAnd e1 e2) = [EAnd e1' e2' | e1' <- shrink e1, e2' <- shrink e2] ++
                        shrink e1 ++
                        shrink e2
  shrink (EOr e1 e2) = [EOr e1' e2' | e1' <- shrink e1, e2' <- shrink e2] ++
                        shrink e1 ++
                        shrink e2
  shrink _ = []


instance Arbitrary ParamDef where
  arbitrary = do sym <- arbitrarySymbolName
                 typ <- arbitrary
                 return $ ParamDef (sym, typ)

  shrink (ParamDef (sym, typ)) = [ParamDef (sym', typ') | sym' <- shrink sym, typ' <- shrink typ]

instance Arbitrary Statement where
  arbitrary = oneof [ SVarDecl <$> arbitrarySymbolName <*> arbitrary
                    , SDefFun <$> arbitrarySymbolName <*> arbitrary <*> arbitrary <*> arbitrary
                    , SSequence <$> (arbitrary `suchThat` (/= SNoOp)) <*> arbitrary
                    , SCall <$> arbitrary <*> arbitrary
                    , SRun <$> arbitrary <*> arbitrary
                    , SReturn <$> arbitrary
                    , pure SNoOp
                    ]

  shrink (SVarDecl name expr) | (length name > 1) = [SVarDecl "x" expr]
                              | otherwise = [SVarDecl name expr' | expr' <- shrink expr]
  shrink (SDefFun name ps rt body) | (length name > 1) = [SDefFun "f" ps rt body]
                                   | (length ps > 1) = [SDefFun name ps' rt body | ps' <- shrink ps]
                                   | otherwise = [SDefFun name ps rt' body' | rt' <- shrink rt, body' <- shrink body]
  shrink (SSequence s1 s2) = [s1, s2]
  shrink (SCall fn ps) = [SCall fn ps' | ps' <- shrink ps] ++
                         [SCall fn' ps | fn' <- shrink fn]
  shrink (SRun fn ps) = [SRun fn ps' | ps' <- shrink ps] ++
                        [SRun fn' ps | fn' <- shrink fn]
  shrink (SReturn e) = [SReturn e' | e' <- shrink e]
  shrink _ = []

printedTypeIsParsable :: Type -> Property
printedTypeIsParsable t = counterexample (pprint t) $ case parseType (pprint t) of
                           Tr.Success t' -> t === t'
                           Tr.Failure err -> error $ "Failed: " ++ show err

printedExpressionIsParsable :: Expression -> Property
printedExpressionIsParsable e = counterexample (pprint e) $
                                  case parseExpr (pprint e) of
                                    Tr.Success e' -> e === e'
                                    Tr.Failure err -> error $ "Failed: " ++ show err

printedStatementIsParsable :: Statement -> Property
printedStatementIsParsable s = counterexample (pprint s) $
                                 case parseMes (pprint s) of
                                   Tr.Success s' -> s === (sStatement s')
                                   Tr.Failure err -> error $ "Failed: " ++ show err

check :: Testable prop => prop -> IO ()
check prop = do
  let args = stdArgs { chatty = True
                     , maxSuccess = 100
                     , maxSize = 25
                     }
  r <- verboseCheckWithResult args prop
  if not (isSuccess r)
    then exitWith $ ExitFailure 1
    else return ()


main :: IO ()
main = do
  putStrLn "Running checks..."
  check printedTypeIsParsable
  check printedExpressionIsParsable
  check printedStatementIsParsable
