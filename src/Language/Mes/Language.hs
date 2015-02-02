module Language.Mes.Language where

type SymbolName = String

data Type = TUnit
          | TString
          | TFun [Type] Type
          deriving (Show, Eq)

data Expression = EStringLit String
                | EVar SymbolName
                | ESysVar SymbolName
                | EApply Expression [Expression]
                  deriving (Show, Eq)

newtype ParamDef = ParamDef (SymbolName, Type)
    deriving (Show, Eq)

data Statement = SVarDecl SymbolName Expression
               | SDefFun SymbolName [ParamDef] Type Statement
               | SSequence Statement Statement
               | SCall Expression [Expression]
               | SRun Expression [Expression] -- TODO: this should be an expression returning a process value
               | SReturn Expression
               | SNoOp
                 deriving (Show, Eq)

sequence :: [Statement] -> Statement
sequence = foldl SSequence SNoOp

data Script = Script { sStatement :: Statement }
