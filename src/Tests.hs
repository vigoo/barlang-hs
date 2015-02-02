import Control.Applicative
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
arbitrarySymbolName = listOf1 arbitrary

instance Arbitrary Type where
  arbitrary = oneof [ pure TUnit
                    , pure TString
                    , TFun <$> (listOf1 arbitrary) <*> arbitrary
                    ]

instance Arbitrary Expression where
  arbitrary = oneof [ EStringLit <$> arbitrary
                    , EVar <$> arbitrarySymbolName
                    , ESysVar <$> arbitrarySymbolName
                    , EApply <$> arbitrary <*> arbitrary
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
printedTypeIsParsable t = case parseType (pprint t) of
                           Right t' -> t === t'
                           Left err -> error $ "Failed: " ++ show err

main :: IO ()
main = do putStrLn "Running checks..."
          r <- verboseCheckResult printedTypeIsParsable
          if isSuccess r
          then exitWith $ ExitSuccess
          else exitWith $ ExitFailure 1
