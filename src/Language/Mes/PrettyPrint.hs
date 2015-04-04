{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Language.Mes.PrettyPrint(PrettyPrint(..)
                               ,escape
                               ,escapeChar
                               ) where

import Language.Mes.Language

import Code.Build

escapeChar :: Char -> String
escapeChar ch = case ch of
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '"' -> "\\\""
  '\\' -> "\\\\"
  _ -> [ch]

escape :: String -> String
escape = concat . (map escapeChar)

parensIfNotVar :: Expression -> Code
parensIfNotVar expr = case expr of
  EVar _ -> code expr
  _ -> parenthesis (code expr)

instance Codeable Type where
    code = \case
             TUnit -> code "unit"
             TString -> code "string"
             TBool -> code "bool"
             TFun types rett -> (parenthesis $ interleave ", " (codeList types)) <++> "->" <++> rett

instance Codeable Expression where
    code = \case
             EStringLit str -> surround "\"" "\"" (escape str)
             EBoolLit True -> code "true"
             EBoolLit False -> code "false"
             EVar sym -> code sym
             ESysVar sym -> "$"  <+> sym
             EApply fnExpr pExprs -> parenthesis $ fnExpr <+> parenthesis (interleave ", " (codeList  pExprs))
             EAnd a b -> parenthesis $ (parenthesis $ code a) <++> "and" <++> (parenthesis $ code b)
             EOr a b -> parenthesis $ (parenthesis $ code a) <++> "or" <++> (parenthesis $ code b)

instance Codeable ParamDef where
    code (ParamDef (name, typ)) = name <+> ": " <+> typ

instance Codeable Statement where
    code = \case
             SVarDecl sym expr ->  "val" <++> sym <++> "=" <++> expr <+> ";"
             SDefFun sym pdefs rett body -> ("def" <++> sym <+> parenthesis (interleave ", " (codeList pdefs)) <+> ":" <++> rett) <-> indent 4 body <-> "end;"
             SSequence s1 s2 -> s1 <-> s2
             SCall fnExpr pExprs -> (parensIfNotVar fnExpr) <++> parenthesis (interleave ", " (codeList pExprs)) <+> ";"
             SRun runExpr pExprs -> "> " <+> runExpr <++> (interleave " " (codeList pExprs)) <+> ";"
             SReturn expr -> "return" <++> expr <+> ";"
             SNoOp -> noCode


class PrettyPrint a where
  pprint :: a -> String

instance PrettyPrint Type where
  pprint = showCode . code

instance PrettyPrint Expression where
  pprint = showCode . code

instance PrettyPrint Statement where
  pprint = showCode . code

instance PrettyPrint Script where
  pprint script = showCode $ code $ sStatement script
