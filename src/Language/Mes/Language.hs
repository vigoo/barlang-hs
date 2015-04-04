module Language.Mes.Language where

type SymbolName = String

data Type = TUnit
          | TString
          | TBool
          | TFun [Type] Type
          deriving (Show, Eq)

data Expression = EStringLit String
                | EBoolLit Bool
                | EVar SymbolName
                | ESysVar SymbolName
                | EApply Expression [Expression]
                | EAnd Expression Expression
                | EOr Expression Expression
                  deriving (Show, Eq)

newtype ParamDef = ParamDef (SymbolName, Type)
    deriving (Show, Eq)

data SingleStatement = SSVarDecl SymbolName Expression
                     | SSDefFun SymbolName [ParamDef] Type Statement
                     | SSCall Expression [Expression]
                     | SSRun Expression [Expression] -- TODO: this should be an expression returning a process value
                     | SSReturn Expression
                     deriving (Show, Eq)


data Statement = SVarDecl SymbolName Expression
               | SDefFun SymbolName [ParamDef] Type Statement
               | SSequence Statement Statement
               | SCall Expression [Expression]
               | SRun Expression [Expression] -- TODO: this should be an expression returning a process value
               | SReturn Expression
               | SNoOp
                 deriving (Show)

normalize :: Statement -> [SingleStatement]
normalize (SVarDecl s e) = [SSVarDecl s e]
normalize (SDefFun s p t b) = [SSDefFun s p t b]
normalize (SCall n p) = [SSCall n p]
normalize (SRun n p) = [SSRun n p]
normalize (SReturn e) = [SSReturn e]
normalize (SNoOp) = []
normalize (SSequence s1 s2) = concatMap normalize [s1, s2]

instance Eq Statement where
  s1 == s2 = normalize s1 == normalize s2

sequence :: [Statement] -> Statement
sequence = foldl SSequence SNoOp

data Script = Script { sStatement :: Statement }
            deriving Show
