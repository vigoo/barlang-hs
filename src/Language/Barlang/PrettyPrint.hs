{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Language.Barlang.PrettyPrint(PrettyPrint(..)
                               ,escape
                               ,escapeChar
                               ) where

import           Language.Barlang.Language

import           Code.Build

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

instance Codeable TypeParam where
  code (TypeParam sym _) = code sym

instance Codeable Type where
    code = \case
             TUnit -> code "unit"
             TString -> code "string"
             TBool -> code "bool"
             TInt -> code "int"
             TDouble -> code "double"
             TFun tps types rett ->
               case tps of
                [] -> body
                _ -> (square $ interleave ", " (codeList tps)) <+> body
               where body = (parenthesis $ interleave ", " (codeList types)) <++> "->" <++> rett
             TVar tp -> code tp

instance Codeable Expression where
    code = \case
             EStringLit str -> surround "\"" "\"" (escape str)
             EBoolLit True -> code "true"
             EBoolLit False -> code "false"
             EIntLit n -> code (show n)
             EDoubleLit n -> code (show n)
             EVar sym -> code sym
             ESysVar sym -> "$"  <+> sym
             EApply fnExpr pExprs -> parenthesis $ fnExpr <+> parenthesis (interleave ", " (codeList  pExprs))
             EAnd a b -> binary "and" a b
             EOr a b -> binary "or" a b
             ENot e -> code "not" <++> (parenthesis $ code e)
             EAdd a b -> binary "+" a b
             ESub a b -> binary "-" a b
             EMul a b -> binary "*" a b
             EDiv a b -> binary "/" a b
             EEq a b -> binary "==" a b
             ENeq a b -> binary "!=" a b
             ELess a b -> binary "<" a b
             ELessEq a b -> binary "<=" a b
             EGreater a b -> binary ">" a b
             EGreaterEq a b -> binary ">=" a b
       where
         binary op a b = parenthesis $ (parenthesis $ code a) <++> op <++> (parenthesis $ code b)

instance Codeable ParamDef where
    code (ParamDef (name, typ)) = name <+> ": " <+> typ

instance Codeable Statement where
    code = \case
             SVarDecl sym expr ->  "val" <++> sym <++> "=" <++> expr <+> ";"
             SDefFun sym tps pdefs rett body -> ("def" <++> symWithTps <+> parenthesis (interleave ", " (codeList pdefs)) <+> ":" <++> rett) <-> indent 4 body <-> "end;"
               where
                 symWithTps = case tps of
                   [] -> code sym
                   _ -> sym <+> square (interleave ", " (codeList tps))
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
