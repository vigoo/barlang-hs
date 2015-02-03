import Control.Applicative
import Control.Monad
import Data.Char
import Debug.Trace
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Test

import Language.Mes.Language
import Language.Mes.Parser
import Language.Mes.PrettyPrint

-- Arbitrary instances
arbitrarySymbolName :: Gen SymbolName
arbitrarySymbolName = listOf1 (arbitrary `suchThat` (\c -> isAscii c && isAlpha c))

instance Arbitrary Type where
  arbitrary = sized arbType
   where
     arbType :: Int -> Gen Type
     arbType 0 = oneof [ pure TUnit
                           , pure TString
                           ]
     arbType n = oneof [ pure TUnit
                       , pure TString
                       , do k <- choose (0, n-1)
                            l <- choose (0, n-1)
                            m <- choose (0, n-1)
                            args <- replicateM k $ arbType l
                            rettype <- arbType m
                            return $ TFun args rettype
                       ]

instance Arbitrary Expression where
  arbitrary = sized arbExpr
    where
      arbExpr :: Int -> Gen Expression
      arbExpr 0 = oneof [ EStringLit <$> (listOf $ arbitrary `suchThat` (not . ((flip elem) "\"\n")))
                        , EVar <$> arbitrarySymbolName
                        , ESysVar <$> arbitrarySymbolName
                        ]
      arbExpr n = oneof [ EStringLit <$> arbitrary
                        , EVar <$> arbitrarySymbolName
                        , ESysVar <$> arbitrarySymbolName
                        , do k <- choose (0, n-1)
                             l <- choose (0, n-1)
                             args <- replicateM k $ arbExpr l
                             fn <- EVar <$> arbitrarySymbolName
                             return $ EApply fn args
                        ]

instance Arbitrary ParamDef where
  arbitrary = do sym <- arbitrarySymbolName
                 typ <- arbitrary
                 return $ ParamDef (sym, typ)

instance Arbitrary Statement where
  arbitrary = oneof [ SVarDecl <$> arbitrarySymbolName <*> arbitrary
                    , SDefFun <$> arbitrarySymbolName <*> arbitrary <*> arbitrary <*> arbitrary
                    , SSequence <$> (arbitrary `suchThat` (/= SNoOp)) <*> arbitrary
                    , SCall <$> arbitrary <*> arbitrary
                    , SRun <$> arbitrary <*> arbitrary
                    , SReturn <$> arbitrary
                    , pure SNoOp
                    ]

printedTypeIsParsable :: Type -> Property
printedTypeIsParsable t = counterexample (pprint t) $ case parseType (pprint t) of
                           Right t' -> t === t'
                           Left err -> error $ "Failed: " ++ show err

printedExpressionIsParsable :: Expression -> Property
printedExpressionIsParsable e = counterexample (pprint e) $
                                  case parseExpr (pprint e) of
                                    Right e' -> e === e'
                                    Left err -> error $ "Failed: " ++ show err

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
