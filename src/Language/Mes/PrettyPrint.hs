{-# LANGUAGE LambdaCase #-}

module Language.Mes.PrettyPrint(PrettyPrint(..)) where

import Language.Mes.Language

import Code.Build

instance Codeable Type where
    code = \case
             TUnit -> code "()"
             TString -> code "string"
             TFun types rett -> (parenthesis $ interleave ", " (codeList types)) <++> "->" <++> rett

instance Codeable Expression where
    code = \case
             EStringLit str -> surround "\"" "\"" str
             EVar sym -> code sym
             ESysVar sym -> "$"  <+> sym
             EApply fnExpr pExprs -> fnExpr <+> parenthesis (interleave ", " (codeList  pExprs))

instance Codeable ParamDef where
    code (ParamDef (name, typ)) = name <+> ": " <+> typ

instance Codeable Statement where
    code = \case
             SVarDecl sym expr ->  "val" <++> sym <++> "=" <++> expr
             SDefFun sym pdefs rett body -> ("def" <++> sym <+> parenthesis (interleave ", " (codeList pdefs)) <+> ":" <++> rett) <-> indent 4 body <-> "end"
             SSequence s1 s2 -> s1 <-> s2
             SCall fnExpr pExprs -> fnExpr <++> parenthesis (interleave ", " (codeList pExprs))
             SRun runExpr pExprs -> "`" <+> runExpr <++> (interleave " " (codeList pExprs)) <+> "`"
             SReturn expr -> "return" <++> expr
             SNoOp -> noCode


class PrettyPrint a where
  pprint :: a -> String

instance PrettyPrint Type where
  pprint = showCode . code

instance PrettyPrint Script where
  pprint script = showCode $ code $ sStatement script
