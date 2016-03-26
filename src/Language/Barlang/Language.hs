module Language.Barlang.Language where

type SymbolName = String

data TypeParam = TypeParam SymbolName
               deriving (Show, Eq)

data Type = TUnit
          | TString
          | TBool
          | TInt
          | TDouble
          | TFun [TypeParam] [Type] Type
          | TVar SymbolName
          | TArray Type
          deriving (Show, Eq)

data UnaryOperator = UONot
                     deriving (Show, Eq)

data BinaryOperator = BOAnd
                    | BOOr
                    | BOAdd
                    | BOSub
                    | BOMul
                    | BODiv
                    | BOMod
                    | BOEq
                    | BONeq
                    | BOLess
                    | BOLessEq
                    | BOGreater
                    | BOGreaterEq
                    deriving (Show, Eq)

data Expression = EStringLit String
                | EBoolLit Bool
                | EIntLit Int
                | EDoubleLit Double
                | EVar SymbolName
                | EArrayAccess SymbolName Expression
                | ESysVar SymbolName
                | EPredefined SymbolName
                | EApply Expression [Expression]
                | EUnaryOp UnaryOperator Expression
                | EBinOp BinaryOperator Expression Expression
                | ELambda [TypeParam] [ParamDef] Type Statement
                  deriving (Show, Eq)

newtype ParamDef = ParamDef (SymbolName, Type)
    deriving (Show, Eq)

data FunProps = FunProps { fpInline :: Bool }
                deriving (Show, Eq)

defaultFunProps :: FunProps
defaultFunProps =
  FunProps { fpInline = False }

data SingleStatement = SSVarDecl SymbolName Expression
                     | SSDefFun SymbolName FunProps [TypeParam] [ParamDef] Type Statement
                     | SSCall Expression [Expression]
                     | SSRun Expression [Expression] -- TODO: this should be an expression returning a process value
                     | SSIf Expression Statement Statement
                     | SSWhile Expression Statement
                     | SSUpdateVar SymbolName Expression
                     | SSUpdateCell SymbolName Expression Expression
                     | SSArrayDecl SymbolName Type
                     | SSReturn Expression
                     deriving (Show, Eq)


data Statement = SVarDecl SymbolName Expression
               | SDefFun SymbolName FunProps [TypeParam] [ParamDef] Type Statement
               | SSequence Statement Statement
               | SCall Expression [Expression]
               | SRun Expression [Expression] -- TODO: this should be an expression returning a process value
               | SReturn Expression
               | SIf Expression Statement Statement
               | SWhile Expression Statement
               | SUpdateVar SymbolName Expression
               | SUpdateCell SymbolName Expression Expression
               | SArrayDecl SymbolName Type
               | SNoOp
                 deriving (Show)

normalize :: Statement -> [SingleStatement]
normalize (SVarDecl s e) = [SSVarDecl s e]
normalize (SDefFun s props tp p t b) = [SSDefFun s props tp p t b]
normalize (SCall n p) = [SSCall n p]
normalize (SRun n p) = [SSRun n p]
normalize (SReturn e) = [SSReturn e]
normalize (SIf c t f) = [SSIf c t f]
normalize (SWhile c b) = [SSWhile c b]
normalize (SUpdateVar s e) = [SSUpdateVar s e]
normalize (SUpdateCell s i e) = [SSUpdateCell s i e]
normalize (SArrayDecl s t) = [SSArrayDecl s t]
normalize (SNoOp) = []
normalize (SSequence s1 s2) = concatMap normalize [s1, s2]

instance Eq Statement where
  s1 == s2 = normalize s1 == normalize s2

sequence :: [Statement] -> Statement
sequence = foldl SSequence SNoOp

data Script = Script { sStatement :: Statement }
            deriving Show
