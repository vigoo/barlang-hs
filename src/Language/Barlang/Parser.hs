{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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

newtype BarlangParser a = BarlangParser { asParser :: Parser a }
  deriving (Functor, Applicative, Monad, Alternative, Parsing, CharParsing)

instance TokenParsing BarlangParser where
    someSpace = buildSomeSpaceParser (void space) scalaCommentStyle

idStyle :: IdentifierStyle BarlangParser
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
                                             , "array"
                                             , "==", "!=", "<", ">", "<=", ">="
                                             , "*", "/", "-", "+", "mod", "<>"
                                             ]
                      }
  where
    set = HashSet.fromList

identifier :: BarlangParser String
identifier = ident idStyle

reserved :: String -> BarlangParser ()
reserved = reserve idStyle

-- Types

typeUnit :: BarlangParser Type
typeUnit = token $ reserved "unit" >> return TUnit

typeString :: BarlangParser Type
typeString = token $ reserved "string" >> return TString

typeBool :: BarlangParser Type
typeBool = token $ reserved "bool" >> return TBool

typeInt :: BarlangParser Type
typeInt = token $ reserved "int" >> return TInt

typeDouble :: BarlangParser Type
typeDouble = token $ reserved "double" >> return TDouble

typeArray :: BarlangParser Type
typeArray = try $ do
  elemType <- brackets (spaces >> typeExpr)
  whiteSpace
  return $ TArray elemType

typeFunArrow :: BarlangParser ()
typeFunArrow = token $ reserved "->"

typeFunParams :: BarlangParser [Type]
typeFunParams = parens $ commaSep typeExpr <?> "function parameter type list"

typeParam :: BarlangParser TypeParam
typeParam = TypeParam <$> identifier <?> "type parameter"

typeParams :: BarlangParser [TypeParam]
typeParams = option [] $ brackets $ commaSep typeParam <?> "type parameter list"

typeFun :: BarlangParser Type
typeFun = TFun <$> typeParams
               <*> typeFunParams
               <*> (typeFunArrow *> typeExpr <?> "function return type")
               <?> "function type"

typeVar :: BarlangParser Type
typeVar = TVar <$> identifier <?> "type variable"

typeExpr :: BarlangParser Type
typeExpr = choice [ try typeFun
                  , typeArray
                  , typeUnit
                  , typeString
                  , typeBool
                  , typeInt
                  , typeDouble
                  , typeVar
                  ]
             <?> "type"

-- Expressions
stringLit :: BarlangParser Expression
stringLit = EStringLit <$> stringLiteral

boolLit :: BarlangParser Expression
boolLit = EBoolLit <$> (highlight Constant $ true <|> false) <?> "boolean literal"
  where
    true = token $ reserved "true" >> return True
    false = token $ reserved "false" >> return False

numLit :: BarlangParser Expression
numLit = (integerOrDouble >>= \case
  Left i -> return $ EIntLit $ fromInteger i
  Right d -> return $ EDoubleLit d) <?> "numeric literal"

variable :: BarlangParser Expression
variable = EVar <$> identifier <?> "variable identifier"

systemVariable :: BarlangParser Expression
systemVariable = ESysVar <$> (highlight Identifier $ char '$' *> identifier) <?> "system variable"

arrayAccess :: BarlangParser Expression
arrayAccess = try $ EArrayAccess <$> identifier <*> brackets (expression False)

funApplication :: BarlangParser Expression
funApplication = try $ EApply <$> variable <*> paramList
  where
    paramList = parens $ commaSep (expression False)

defKeyword :: BarlangParser ()
defKeyword = token $ reserved "def"

ifKeyword :: BarlangParser ()
ifKeyword = token $ reserved "if"

thenKeyword :: BarlangParser ()
thenKeyword = token $ reserved "then"

elseKeyword :: BarlangParser ()
elseKeyword = token $ reserved "else"

whileKeyword :: BarlangParser ()
whileKeyword = token $ reserved "while"

fnKeyword :: BarlangParser ()
fnKeyword = token $ reserved "fn"

endKeyword :: BarlangParser ()
endKeyword = token $ reserved "end"

typSepSym :: BarlangParser ()
typSepSym = token $ reserved ":"

paramDef :: BarlangParser ParamDef
paramDef = do name <- identifier
              typSepSym
              typ <- typeExpr
              return $ ParamDef (name, typ)

paramDefs :: BarlangParser [ParamDef]
paramDefs = parens $ commaSep paramDef <?> "parameter list definition"

body :: BarlangParser () -> BarlangParser [Statement]
body fin = (try fin >> return [])
         <|> do x <- statement <* semi
                xs <- body fin
                return (x:xs)

lambda :: BarlangParser Expression
lambda = try $ ELambda <$> (fnKeyword *> typeParams) <*> paramDefs <*> retType <*> (collapse <$> body endKeyword <?> "lambda function body")
  where
    retType = typSepSym *> typeExpr <?> "return type definition"

term :: Bool -> BarlangParser Expression
term parfun = parens (expression False)
        <|> lambda
        <|> stringLit
        <|> boolLit
        <|> numLit
        <|> arrayAccess
        <|> (if parfun then parens funApplication else funApplication)
        <|> variable
        <|> systemVariable

exprTable :: [[Operator BarlangParser Expression]]
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
              ,Infix (pure (EBinOp BOMod) <* token (reserved "mod")) AssocLeft
              ]
            , [Infix (pure (EBinOp BOAdd) <* token (reserved "+")) AssocLeft
              ,Infix (pure (EBinOp BOSub) <* token (reserved "-")) AssocLeft
              ]
            ]

expression :: Bool -> BarlangParser Expression
expression parfun = buildExpressionParser exprTable (term parfun) <?> "expression"

-- Statements

nop :: BarlangParser Statement
nop = pure SNoOp

ret :: BarlangParser Statement
ret = SReturn <$> (returnKeyword >> expression False) <?> "return statement"
  where
    returnKeyword = token $ reserved "return"

val :: BarlangParser Statement
val = SVarDecl <$> (valKeyword *> identifier <* equals) <*> expression False <?> "variable declaration"
  where
    valKeyword = token $ reserved "val"
    equals = token $ reserved "="

ifthenelse :: BarlangParser Statement
ifthenelse =     try (SIf <$> (ifKeyword *> expression False <* thenKeyword) <*> (collapse <$> body endKeyword) <*> pure SNoOp)
             <|> (SIf <$> (ifKeyword *> expression False <* thenKeyword) <*> (collapse <$> body elseKeyword) <*> (collapse <$> body endKeyword))
             <?> "conditional statement"

while :: BarlangParser Statement
while =     try (SWhile <$> (whileKeyword *> expression False <?> "loop condition") <*> (collapse <$> body endKeyword))
        <?> "while loop"

arrayDecl :: BarlangParser Statement
arrayDecl = try $ do
  _ <- text "array"
  elemType <- brackets (spaces >> typeExpr)
  whiteSpace
  name <- identifier
  return $ SArrayDecl name elemType

call :: BarlangParser Statement
call = try (SCall <$> ((expression True <?> "function expression") <|> (variable <?> "function name"))
                  <*> paramList
           ) <?> "function call"
  where
    paramList = parens (commaSep (expression False)) <?> "function call's parameter list"

run :: BarlangParser Statement
run = (token $ reserved ">") >> (SRun <$> (expression True <?> "program name")
                                      <*> runParams <?> "program parameter list"
                                ) <?> "run statement"
  where
    runParams = many $ expression True

deffun :: BarlangParser Statement
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

varUpdate :: BarlangParser Statement
varUpdate =     try (SUpdateVar <$> identifier <*> (token (reserved "<-") *> expression False) <?> "variable update")
            <|> try (SUpdateCell <$> identifier <*> brackets (spaces >> expression False) <*> (token (reserved "<-") *> expression False))

statement :: BarlangParser Statement
statement = choice [ ret
                   , run
                   , varUpdate
                   , val
                   , arrayDecl
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

statements :: BarlangParser Statement
statements = collapse <$> statement `endBy` semi <?> "statements"

script :: BarlangParser Script
script = Script <$> (whiteSpace *> statements <* eof) <?> "barlang script"

showParseError :: (MonadIO m) => Pretty.Doc -> m ()
showParseError xs = liftIO $ Pretty.displayIO stdout $ Pretty.renderPretty 0.8 80 $ xs <> Pretty.linebreak

parse :: BarlangParser a -> String -> Result a
parse (BarlangParser p) = parseString p (Columns 0 0)

parseType :: String -> Result Type
parseType = parse typeExpr

parseExpr :: String -> Result Expression
parseExpr = parse $ expression False

parseBarlang :: String -> Result Script
parseBarlang = parse script

parseBarlangFile :: FilePath -> IO (Result Script)
parseBarlangFile = parseFromFileEx (asParser script)

