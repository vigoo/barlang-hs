module Language.Mes.Language where

type SymbolName = String

data Type = TUnit
          | TString
          | TFun [Type] Type
          deriving (Show, Eq)

data Expression = EStringLit String
                | EVar SymbolName
                | ESysVar SymbolName
                | EFunRef SymbolName
                | EApply Expression [Expression]
                  deriving (Show)

newtype ParamDef = ParamDef (SymbolName, Type)
    deriving (Show)

data Statement = SVarDecl SymbolName Expression
               | SDefFun SymbolName [ParamDef] Type Statement
               | SSequence Statement Statement
               | SCall Expression [Expression]
               | SRun Expression [Expression] -- TODO: this should be an expression returning a process value
               | SReturn Expression
               | SNoOp
                 deriving (Show)

sequence :: [Statement] -> Statement
sequence = foldl SSequence SNoOp

data Script = Script { sStatement :: Statement }
