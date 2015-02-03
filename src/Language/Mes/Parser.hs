{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Language.Mes.Parser where

import Control.Applicative hiding ((<|>), many)
import Data.Char
import Text.Parsec
import Text.Parsec.String

import Language.Mes.Language

-- Basic
escapedChar :: Parser Char
escapedChar = char '\\' >> choice [char i >> return o | (i, o) <- mapping]
  where
    mapping = [ ('n', '\n')
              , ('r', '\r')
              , ('t', '\t')
              , ('"', '"')
              , ('\\', '\\')
              ]

stringChar :: Parser Char
stringChar = try escapedChar <|> noneOf "\""

symbolStart :: Parser Char
symbolStart = letter

symbolLetter :: Parser Char
symbolLetter = alphaNum

isReserved :: SymbolName -> Bool
isReserved "val" = True
isReserved "return" = True
isReserved _ = False

symbolName :: Parser SymbolName
symbolName = do sym <- symbolName'
                if (isReserved sym)
                then unexpected $ "reserved name: " ++ sym
                else return sym
    where
      symbolName' = do x <- symbolStart
                       xs <- many symbolLetter
                       return (x:xs)
                    <?> "symbol name"


seps :: Parser ()
seps = skipMany1 (satisfy isSeparator)

seps0 :: Parser ()
seps0 = skipMany (satisfy isSeparator)

-- Types

typeUnit :: Parser Type
typeUnit = string "unit" *> pure TUnit

typeString :: Parser Type
typeString = string "string" *> pure TString

typeFun :: Parser Type
typeFun = TFun <$> typeFunParams <*> (typeFunArrow *> typeExpr)
  where
    typeFunParams :: Parser [Type]
    typeFunParams = between (char '(') (char ')') (typeExpr `sepBy` ((char ',') *> seps0))

    typeFunArrow = seps0 *> string "->" *> seps0

typeExpr :: Parser Type
typeExpr = choice [typeUnit, typeString, typeFun]

-- Expressions
stringLit :: Parser Expression
stringLit = EStringLit <$> (between (char '"') (char '"') (many stringChar))

variable :: Parser Expression
variable = EVar <$> symbolName

systemVariable :: Parser Expression
systemVariable = ESysVar <$> (char '$' *> symbolName)

funApplication :: Parser Expression
funApplication = EApply <$> (variable <* seps0 <* char '(') <*> ((expression `sepBy` ((char ',') *> seps0)) <* char ')')

expression :: Parser Expression
expression =  stringLit
          <|> systemVariable
          <|> try funApplication
          <|> variable

-- Statements
nop :: Parser Statement
nop = spaces *> eof *> pure SNoOp

sequenceP :: Parser Statement
sequenceP = SSequence <$> statement <*> (many1 endOfLine *> statements)

ret :: Parser Statement
ret = SReturn <$> (string "return" *> seps *> expression)

vardecl :: Parser Statement
vardecl = SVarDecl <$> (string "val" *> seps *> symbolName <* seps <* char '=' <* seps) <*> expression

call :: Parser Statement
call = SCall <$> (variable <* seps0 <* char '(') <*> ((expression `sepBy` ((char ',') *> seps0)) <* char ')')

paramDefs :: Parser [ParamDef]
paramDefs = pure [] -- TODO

deffun :: Parser Statement
deffun = SDefFun <$> (string "def" *> seps *> symbolName <* seps0 <* char '(' <* seps0) <*> (paramDefs <* seps0 <* char ')' <* seps0) <*> (seps0 *> char ':' *> typeExpr) <*> (pure SNoOp) -- TODO

statements :: Parser Statement
statements =  try sequenceP
          <|> statement

statement :: Parser Statement
statement =  ret
         <|> vardecl
         <|> deffun
         <|> call
         <|> nop

parser :: Parser Statement
parser = statements

parseType :: String -> Either ParseError Type
parseType = runParser typeExpr () "source"

parseExpr :: String -> Either ParseError Expression
parseExpr = runParser expression () "source"

parseMes :: String -> Either ParseError Statement
parseMes = runParser parser () "source"
