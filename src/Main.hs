{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (sequence)

import Control.Applicative
import Options.Applicative hiding (Success, Failure)
import Text.Trifecta.Result

import Language.Mes.Compiler
import Language.Mes.Language
import Language.Mes.Parser
import Language.Mes.PrettyPrint

data Parameters = Parameters { pSource :: FilePath
                             }


parameters :: Parser Parameters
parameters = Parameters
             <$> strArgument (metavar "MES")

withParameters :: (Parameters -> IO ()) -> IO ()
withParameters fn = execParser opts >>= fn
  where
    opts = info (helper <*> parameters)
           (  fullDesc
           <> progDesc "MES compiler"
           <> header "mes"
           )

run :: Parameters -> IO ()
run Parameters{..} = do
  result <- parseMesFile pSource
  case result of
   Failure xs -> showParseError xs
   Success mes -> do
     let bash = compileToString mes
     putStrLn bash

main :: IO ()
main = withParameters run
