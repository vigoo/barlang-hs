{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Barlang.Test.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           System.Exit
import           Test.QuickCheck
import           Test.QuickCheck.Test

import qualified Text.Trifecta.Result         as Tr

import           Language.Barlang.Language
import           Language.Barlang.Parser
import           Language.Barlang.PrettyPrint

-- Arbitrary instances
arbitrarySymbolName :: Gen SymbolName
arbitrarySymbolName = resize 8 $ listOf1 (arbitrary `suchThat` (\c -> isAscii c && isAlpha c))

arbitraryStringLiteral :: Gen SymbolName
arbitraryStringLiteral = listOf $ arbitrary `suchThat` (\c -> isAscii c && (isAlpha c || c == ' ')) -- TODO

instance Arbitrary TypeParam where
  arbitrary = TypeParam <$> arbitrarySymbolName

instance Arbitrary Type where
  arbitrary = sized arbType
   where
     arbType :: Int -> Gen Type
     arbType 0 = oneof [ pure TUnit
                       , pure TString
                       , pure TBool
                       , pure TInt
                       , pure TDouble
                       , TVar <$> arbitrarySymbolName
                       ]
     arbType n = oneof [ pure TUnit
                       , pure TString
                       , pure TBool
                       , pure TInt
                       , pure TDouble
                       , TVar <$> arbitrarySymbolName
                       , TArray <$> arbitrary
                       , do k <- choose (0, n-1)
                            l <- choose (0, n-1)
                            m <- choose (0, n-1)
                            args <- replicateM k $ arbType l
                            rettype <- arbType m
                            tps <- arbitrary
                            return $ TFun tps args rettype
                       ]

  shrink (TFun tps args rettype) = [TFun tps args' rettype| args' <- shrink args] ++
                                   [TFun tps args rettype'| rettype' <- shrink rettype] ++
                                   [TFun tps' args rettype| tps' <- shrink tps]
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
                        , ELambda <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        ]
      arbExpr n = oneof [ EStringLit <$> arbitraryStringLiteral
                        , EBoolLit <$> arbitrary
                        , EIntLit <$> arbitrary
                        , EDoubleLit <$> arbitrary
                        , EVar <$> arbitrarySymbolName
                        , ESysVar <$> arbitrarySymbolName
                        , EArrayAccess <$> arbitrarySymbolName <*> arbitrary
                        , ELambda <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        , do k <- choose (0, n-1)
                             l <- choose (0, n-1)
                             args <- replicateM k $ arbExpr l
                             fn <- EVar <$> arbitrarySymbolName
                             return $ EApply fn args
                        , binary BOAnd
                        , binary BOOr
                        , binary BOAdd
                        , binary BOSub
                        , binary BOMul
                        , binary BODiv
                        , binary BOEq
                        , binary BONeq
                        , binary BOLess
                        , binary BOLessEq
                        , binary BOGreater
                        , binary BOGreaterEq
                        , unary UONot
                        ]
            where
              unary c = do
                l1 <- choose (0, n-1)
                e1 <- arbExpr l1
                return $ EUnaryOp c e1
              binary c = do
                l1 <- choose (0, n-1)
                l2 <- choose (0, n-1)
                e1 <- arbExpr l1
                e2 <- arbExpr l2
                return $ EBinOp c e1 e2

  shrink (EStringLit _) = [EStringLit "hello"]
  shrink (EIntLit _) = [EIntLit 1]
  shrink (EDoubleLit _) = [EDoubleLit 0.1, EIntLit 2]
  shrink (EVar n) | (length n > 1) = [EVar "x"]
                  | otherwise = []
  shrink (ESysVar n) | (length n > 1) = [ESysVar "V"]
                  | otherwise = []
  shrink (EApply sym args) = [EApply sym' args' | sym' <- shrink sym, args' <- shrink args]
  shrink (EBinOp BOAnd e1 e2) = [EBinOp BOAnd e1' e2' | e1' <- shrink e1, e2' <- shrink e2] ++
                                   shrink e1 ++
                                   shrink e2
  shrink (EBinOp BOOr e1 e2) = [EBinOp BOOr e1' e2' | e1' <- shrink e1, e2' <- shrink e2] ++
                                  shrink e1 ++
                                  shrink e2
  shrink (ELambda tps pdefs rett body) = [ELambda tps' pdefs' rett' body' | tps' <- shrink tps, pdefs' <- shrink pdefs, rett' <- shrink rett, body' <- shrink body]
  shrink _ = []


instance Arbitrary ParamDef where
  arbitrary = do sym <- arbitrarySymbolName
                 typ <- arbitrary
                 return $ ParamDef (sym, typ)

  shrink (ParamDef (sym, typ)) = [ParamDef (sym', typ') | sym' <- shrink sym, typ' <- shrink typ]

instance Arbitrary FunProps where
  arbitrary = FunProps <$> arbitrary

instance Arbitrary Statement where
  arbitrary = oneof [ SVarDecl <$> arbitrarySymbolName <*> arbitrary
                    , SDefFun <$> arbitrarySymbolName <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , SSequence <$> (arbitrary `suchThat` (/= SNoOp)) <*> arbitrary
                    , SCall <$> arbitrary <*> arbitrary
                    , SRun <$> arbitrary <*> arbitrary
                    , SReturn <$> arbitrary
                    , SIf <$> arbitrary <*> arbitrary <*> arbitrary
                    , SWhile <$> arbitrary <*> arbitrary
                    , SUpdateVar <$> arbitrarySymbolName <*> arbitrary
                    , SUpdateCell <$> arbitrarySymbolName <*> arbitrary <*> arbitrary
                    , SArrayDecl <$> arbitrarySymbolName <*> arbitrary
                    , pure SNoOp
                    ]

  shrink (SVarDecl name expr) | (length name > 1) = [SVarDecl "x" expr]
                              | otherwise = [SVarDecl name expr' | expr' <- shrink expr]
  shrink (SDefFun name props tps ps rt body) | (length name > 1) = [SDefFun "f" props tps ps rt body]
                                       | (length ps > 1) = [SDefFun name props tps ps' rt body | ps' <- shrink ps]
                                       | otherwise = [SDefFun name props tps ps rt' body' | rt' <- shrink rt, body' <- shrink body]
  shrink (SSequence s1 s2) = shrink s1 ++
                             shrink s2 ++
                             [SSequence (SReturn (EIntLit 0)) s' | s' <- shrink s1] ++
                             [SSequence s' (SReturn (EIntLit 0)) | s' <- shrink s2]
  shrink (SCall fn ps) = [SCall fn ps' | ps' <- shrink ps] ++
                         [SCall fn' ps | fn' <- shrink fn]
  shrink (SRun fn ps) = [SRun fn ps' | ps' <- shrink ps] ++
                        [SRun fn' ps | fn' <- shrink fn]
  shrink (SReturn e) = [SReturn e' | e' <- shrink e]
  shrink _ = []

class (Eq a, Show a) => ApproxEqProp a where
  infix 4 ==~
  (==~) :: a -> a -> Property

instance ApproxEqProp Expression where
  (EDoubleLit a) ==~ (EDoubleLit b) = counterexample (show a ++ " /= " ++ show b) (abs (a - b) < eps)
    where eps = 1e-6
  EApply a1 a2s ==~ EApply b1 b2s = a1 ==~ b1 .&&. counterexample (show a2s ++ " /= " ++ show b2s) (conjoin $ map (\(a2, b2) -> a2 ==~ b2) (zip a2s b2s))
  EUnaryOp op1 a1 ==~ EUnaryOp op2 b1 = op1 == op2 .&&. a1 ==~ b1
  EBinOp op1 a1 a2 ==~ EBinOp op2 b1 b2 = op1 == op2 .&&. a1 ==~ b1 .&&. a2 ==~ b2
  a ==~ b = a === b

instance ApproxEqProp SingleStatement where
  SSVarDecl n1 e1 ==~ SSVarDecl n2 e2 = n1 == n2 .&&. e1 ==~ e2
  SSDefFun n1 props1 tps1 ps1 t1 b1 ==~ SSDefFun n2 props2 tps2 ps2 t2 b2 = n1 == n2 .&&. props1 == props2 .&&. tps1 == tps2 .&&. ps1 == ps2 .&&. t1 == t2 .&&. b1 ==~ b2
  SSCall e1 e1s ==~ SSCall e2 e2s = e1 ==~ e2 .&&. conjoin (map (uncurry (==~)) (zip e1s e2s))
  SSRun e1 e1s ==~ SSRun e2 e2s = e1 ==~ e2 .&&. conjoin (map (uncurry (==~)) (zip e1s e2s))
  SSReturn e1 ==~ SSReturn e2 = e1 ==~ e2
  SSIf c1 t1 f1 ==~ SSIf c2 t2 f2 = c1 ==~ c2 .&&. t1 ==~ t2 .&&. f1 ==~ f2
  a ==~ b = a === b

instance ApproxEqProp Statement where
  a ==~ b = conjoin (map (uncurry (==~)) (zip (normalize a) (normalize b)))

printedTypeIsParsable :: Type -> Property
printedTypeIsParsable t = counterexample (pprint t) $ case parseType (pprint t) of
                           Tr.Success t' -> t === t'
                           Tr.Failure err -> error $ "Failed: " ++ show err

printedExpressionIsParsable :: Expression -> Property
printedExpressionIsParsable e = counterexample (pprint e) $
                                  case parseExpr (pprint e) of
                                    Tr.Success e' -> e ==~ e'
                                    Tr.Failure err -> error $ "Failed: " ++ show err

printedStatementIsParsable :: Statement -> Property
printedStatementIsParsable s = counterexample (pprint s) $
                                 case parseBarlang (pprint s) of
                                   Tr.Success s' -> s ==~ (sStatement s')
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


