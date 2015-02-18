{-# LANGUAGE OverloadedStrings #-}
module Language.Mes.Parser where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashSet as HashSet
import Data.Semigroup
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta.Delta
import Text.Trifecta.Parser
import Text.Trifecta.Result
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import System.IO

import Language.Mes.Language

idStyle :: IdentifierStyle Parser
idStyle = emptyIdents { _styleReserved = set [ "string"
                                             , "unit"
                                             , "->"
                                             , "return"
                                             , "val"
                                             , "="
                                             , ">"
                                             , "def"
                                             , ":"
                                             , "end"
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

typeFunArrow :: Parser ()
typeFunArrow = token $ reserved "->"

typeFunParams :: Parser [Type]
typeFunParams = parens $ commaSep typeExpr

typeFun :: Parser Type
typeFun = TFun <$> typeFunParams <*> (typeFunArrow *> typeExpr)

typeExpr :: Parser Type
typeExpr = choice [ typeUnit
                  , typeString
                  , typeFun
                  ]

-- Expressions
stringLit :: Parser Expression
stringLit = EStringLit <$> stringLiteral

variable :: Parser Expression
variable = EVar <$> identifier

systemVariable :: Parser Expression
systemVariable = ESysVar <$> (char '$' *> identifier)

funApplication :: Parser Expression
funApplication = try $ EApply <$> variable <*> paramList
  where
    paramList = parens $ commaSep expression

expression :: Parser Expression
expression = choice [ stringLit
                    , funApplication
                    , variable
                    , systemVariable
                    ]

-- Statements

nop :: Parser Statement
nop = pure SNoOp

ret :: Parser Statement
ret = SReturn <$> (returnKeyword >> expression) <?> "return statement"
  where
    returnKeyword = token $ reserved "return"

val :: Parser Statement
val = SVarDecl <$> (valKeyword *> identifier <* equals) <*> expression <?> "variable declaration"
  where
    valKeyword = token $ reserved "val"
    equals = token $ reserved "="

call :: Parser Statement
call = try $ SCall <$> (parens expression <|> variable) <*> paramList
  where
    paramList = parens $ commaSep expression

run :: Parser Statement
run = (token $ reserved ">") >> SRun <$> expression <*> runParams
  where
    runParams = many expression

deffun :: Parser Statement
deffun = SDefFun <$> (defKeyword *> identifier) <*> paramDefs <*> retType <*> (collapse <$> body)
  where
    defKeyword = token $ reserved "def"
    endKeyword = token $ reserved "end"
    typSepSym = token $ reserved ":"
    paramDef = do name <- identifier
                  typSepSym
                  typ <- typeExpr
                  return $ ParamDef (name, typ)
    paramDefs = parens $ commaSep paramDef
    retType = typSepSym *> typeExpr
    body = (try endKeyword >> return [])
           <|> do x <- statement <* semi
                  xs <- body
                  return (x:xs)

statement :: Parser Statement
statement = choice [ ret
                   , run
                   , val
                   , call
                   , deffun
                   , nop
                   ]

collapse :: [Statement] -> Statement
collapse [] = SNoOp
collapse [s] = s
collapse (x:xs) = SSequence x (collapse xs)

statements :: Parser Statement
statements = collapse <$> statement `endBy` semi

showParseError :: (MonadIO m) => Pretty.Doc -> m ()
showParseError xs = liftIO $ Pretty.displayIO stdout $ Pretty.renderPretty 0.8 80 $ xs <> Pretty.linebreak

parse :: Parser a -> String -> Result a
parse p src = parseString p (Columns 0 0) src

parseType :: String -> Result Type
parseType = parse typeExpr

parseExpr :: String -> Result Expression
parseExpr = parse expression

parseMes :: String -> Result Statement
parseMes = parse statements
