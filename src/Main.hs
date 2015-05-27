{-# LANGUAGE RecordWildCards #-}

import           Prelude                      hiding (sequence)

import           Control.Applicative
import           Control.Monad
import           Options.Applicative          hiding (Failure, Success)
import           System.Directory
import           Text.Trifecta.Result

import           Language.Barlang.Compiler
import           Language.Barlang.Parser
import           Language.Barlang.PrettyPrint

data Parameters = Parameters { pSource     :: FilePath
                             , pTarget     :: Maybe FilePath
                             , pDumpAST    :: Bool
                             , pPPrintOnly :: Bool
                             }


parameters :: Parser Parameters
parameters = Parameters
             <$> strArgument (metavar "MES")
             <*> (optional $ strOption
                  ( long "output"
                 <> short 'o'
                 <> metavar "BASH"
                 <> help "Target path for the generated bash script"
                 ))
             <*> switch
                 ( long "dump-ast"
                <> help "If set, the AST is dumped to stdout"
                 )
             <*> switch
                 ( long "pretty-print"
                <> help "If set, the MES source is pretty printed to stdout, no compilation made"
                 )

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
     case pPPrintOnly of
      True -> putStrLn $ pprint mes
      False -> do
        let bash = compileToString mes

        when pDumpAST $ putStrLn (show mes)

        case pTarget of
         Nothing -> putStrLn bash
         Just path -> do
           writeFile path bash
           p <- getPermissions path
           setPermissions path p { executable = True }

main :: IO ()
main = withParameters run
