{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Barlang.Parser ( parseType
                               , parseExpr
                               , parseBarlang
                               , parseBarlangFile
                               , showParseError
                               )
       where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.HashSet                 as HashSet
import           Data.Maybe
import           Data.Monoid
import           System.IO
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty hiding (empty, line,
                                                         (<$>), (<>))
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Language.Barlang.Language

idStyle :: IdentifierStyle Parser
idStyle = emptyIdents { _styleReserved = set [ "string"
                                             , "bool"
                                             , "int"
                                             , "double"
                                             , "unit"
                                             , "->"
                                             , "return"
                                             , "val"
                                             , "="
                                             , ">"
                                             , "def"
                                             , ":"
                                             , "end"
                                             , "inline"
                                             , "if"
                                             , "then"
                                             , "else"
                                             , "fn"
                                             , "true"
                                             , "false"
                                             , "and"
                                             , "or"
                                             , "not"
                                             , "while"
                                             , "<-"
                                             , "==", "!=", "<", ">", "<=", ">="
                                             , "*", "/", "-", "+"
                                             ]
                      }
  where
    set = HashSet.fromList

identifier :: Parser String
identifier = ident idStyle

reserved :: String -> Parser ()
reserved = reserve idStyle

-- Types

typeUnit :: Parser Type
typeUnit = token $ reserved "unit" >> return TUnit

typeString :: Parser Type
typeString = token $ reserved "string" >> return TString

typeBool :: Parser Type
typeBool = token $ reserved "bool" >> return TBool

typeInt :: Parser Type
typeInt = token $ reserved "int" >> return TInt

typeDouble :: Parser Type
typeDouble = token $ reserved "double" >> return TDouble

typeFunArrow :: Parser ()
typeFunArrow = token $ reserved "->"

typeFunParams :: Parser [Type]
typeFunParams = parens $ commaSep typeExpr <?> "function parameter type list"

typeParam :: Parser TypeParam
typeParam = TypeParam <$> identifier <?> "type parameter"

typeParams :: Parser [TypeParam]
typeParams = option [] $ brackets $ commaSep typeParam <?> "type parameter list"

typeFun :: Parser Type
typeFun = TFun <$> typeParams
               <*> typeFunParams
               <*> (typeFunArrow *> typeExpr <?> "function return type")
               <?> "function type"

typeVar :: Parser Type
typeVar = TVar <$> identifier <?> "type variable"

typeExpr :: Parser Type
typeExpr = choice [ typeUnit
                  , typeString
                  , typeBool
                  , typeInt
                  , typeDouble
                  , typeFun
                  , typeVar
                  ]
             <?> "type"

-- Expressions
stringLit :: Parser Expression
stringLit = EStringLit <$> stringLiteral

boolLit :: Parser Expression
boolLit = EBoolLit <$> (highlight Constant $ true <|> false) <?> "boolean literal"
  where
    true = token $ reserved "true" >> return True
    false = token $ reserved "false" >> return False

numLit :: Parser Expression
numLit = (integerOrDouble >>= \case
  Left i -> return $ EIntLit $ fromInteger i
  Right d -> return $ EDoubleLit d) <?> "numeric literal"

variable :: Parser Expression
variable = EVar <$> identifier <?> "variable identifier"

systemVariable :: Parser Expression
systemVariable = ESysVar <$> (highlight Identifier $ char '$' *> identifier) <?> "system variable"

funApplication :: Parser Expression
funApplication = try $ EApply <$> variable <*> paramList
  where
    paramList = parens $ commaSep (expression False)

defKeyword :: Parser ()
defKeyword = token $ reserved "def"

ifKeyword :: Parser ()
ifKeyword = token $ reserved "if"

thenKeyword :: Parser ()
thenKeyword = token $ reserved "then"

elseKeyword :: Parser ()
elseKeyword = token $ reserved "else"

whileKeyword :: Parser ()
whileKeyword = token $ reserved "while"

fnKeyword :: Parser ()
fnKeyword = token $ reserved "fn"

endKeyword :: Parser ()
endKeyword = token $ reserved "end"

typSepSym :: Parser ()
typSepSym = token $ reserved ":"

paramDef :: Parser ParamDef
paramDef = do name <- identifier
              typSepSym
              typ <- typeExpr
              return $ ParamDef (name, typ)

paramDefs :: Parser [ParamDef]
paramDefs = parens $ commaSep paramDef <?> "parameter list definition"

body :: Parser () -> Parser [Statement]
body fin = (try fin >> return [])
         <|> do x <- statement <* semi
                xs <- body fin
                return (x:xs)

lambda :: Parser Expression
lambda = try $ ELambda <$> (fnKeyword *> typeParams) <*> paramDefs <*> retType <*> (collapse <$> body endKeyword <?> "lambda function body")
  where
    retType = typSepSym *> typeExpr <?> "return type definition"

term :: Bool -> Parser Expression
term parfun = parens (expression False)
        <|> lambda
        <|> stringLit
        <|> boolLit
        <|> numLit
        <|> (if parfun then parens funApplication else funApplication)
        <|> variable
        <|> systemVariable

exprTable :: [[Operator Parser Expression]]
exprTable = [ [Infix (pure (EBinOp BOAnd) <* token (reserved "and")) AssocLeft ]
            , [Infix (pure (EBinOp BOOr) <* token (reserved "or")) AssocLeft ]
            , [Prefix (pure (EUnaryOp UONot) <* token (reserved "not"))]
            , [Infix (pure (EBinOp BOEq) <* token (reserved "==")) AssocLeft
              ,Infix (pure (EBinOp BONeq) <* token (reserved "!=")) AssocLeft
              ,Infix (pure (EBinOp BOLessEq) <* token (reserved "<=")) AssocLeft
              ,Infix (pure (EBinOp BOGreaterEq) <* token (reserved ">=")) AssocLeft
              ,Infix (pure (EBinOp BOGreater) <* token (reserved ">")) AssocLeft
              ,Infix (pure (EBinOp BOLess) <* token (reserved "<")) AssocLeft
              ]
            , [Infix (pure (EBinOp BOMul) <* token (reserved "*")) AssocLeft
              ,Infix (pure (EBinOp BODiv) <* token (reserved "/")) AssocLeft
              ]
            , [Infix (pure (EBinOp BOAdd) <* token (reserved "+")) AssocLeft
              ,Infix (pure (EBinOp BOSub) <* token (reserved "-")) AssocLeft
              ]
            ]

expression :: Bool -> Parser Expression
expression parfun = buildExpressionParser exprTable (term parfun) <?> "expression"

-- Statements

nop :: Parser Statement
nop = pure SNoOp

ret :: Parser Statement
ret = SReturn <$> (returnKeyword >> expression False) <?> "return statement"
  where
    returnKeyword = token $ reserved "return"

val :: Parser Statement
val = SVarDecl <$> (valKeyword *> identifier <* equals) <*> expression False <?> "variable declaration"
  where
    valKeyword = token $ reserved "val"
    equals = token $ reserved "="

ifthenelse :: Parser Statement
ifthenelse =     try (SIf <$> (ifKeyword *> expression False <* thenKeyword) <*> (collapse <$> body endKeyword) <*> pure SNoOp)
             <|> (SIf <$> (ifKeyword *> expression False <* thenKeyword) <*> (collapse <$> body elseKeyword) <*> (collapse <$> body endKeyword))
             <?> "conditional statement"

while :: Parser Statement
while =     try (SWhile <$> (whileKeyword *> expression False <?> "loop condition") <*> (collapse <$> body endKeyword))
        <?> "while loop"

call :: Parser Statement
call = try (SCall <$> ((expression True <?> "function expression") <|> (variable <?> "function name"))
                  <*> paramList
           ) <?> "function call"
  where
    paramList = parens (commaSep (expression False)) <?> "function call's parameter list"

run :: Parser Statement
run = (token $ reserved ">") >> (SRun <$> (expression True <?> "program name")
                                      <*> runParams <?> "program parameter list"
                                ) <?> "run statement"
  where
    runParams = many $ expression True

deffun :: Parser Statement
deffun = inlineKeyword >>= \inline ->
  (SDefFun <$> (defKeyword *> identifier <?> "function name")
           <*> pure (defaultFunProps { fpInline = inline })
           <*> typeParams
           <*> paramDefs
           <*> retType
           <*> (collapse <$> body endKeyword <?> "function body")
  ) <?> "function definition"

  where
    inlineKeyword = liftM (fromMaybe False) $ (optional ((token $ reserved "inline") >> return True)) <?> "inline keyword"
    retType = typSepSym *> typeExpr <?> "return type definition"

varUpdate :: Parser Statement
varUpdate = try $ SUpdateVar <$> identifier <*> (token (reserved "<-") *> expression False) <?> "variable update"

statement :: Parser Statement
statement = choice [ ret
                   , run
                   , varUpdate
                   , val
                   , ifthenelse
                   , while
                   , call
                   , deffun
                   , nop
                   ]
            <?> "statement"

collapse :: [Statement] -> Statement
collapse [] = SNoOp
collapse [s] = s
collapse (x:xs) = SSequence x (collapse xs)

statements :: Parser Statement
statements = collapse <$> statement `endBy` semi <?> "statements"

script :: Parser Script
script = (Script <$> statements <* eof) <?> "barlang script"

showParseError :: (MonadIO m) => Pretty.Doc -> m ()
showParseError xs = liftIO $ Pretty.displayIO stdout $ Pretty.renderPretty 0.8 80 $ xs <> Pretty.linebreak

parse :: Parser a -> String -> Result a
parse p = parseString p (Columns 0 0)

parseType :: String -> Result Type
parseType = parse typeExpr

parseExpr :: String -> Result Expression
parseExpr = parse $ expression False

parseBarlang :: String -> Result Script
parseBarlang = parse script

parseBarlangFile :: FilePath -> IO (Result Script)
parseBarlangFile = parseFromFileEx script
