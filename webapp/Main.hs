{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Debug.Trace
import           GHCJS.Foreign.Callback
import           GHCJS.Prim
import           Language.Barlang.Compiler
import           Language.Barlang.Optimizer
import           Language.Barlang.Parser
import           Text.Trifecta.Result

compileBarlangSource :: String -> IO (Either String String)
compileBarlangSource src = do
    let result = parseBarlang src
    case result of
        Failure xs ->
            return $ Left (parseErrorAsString xs)
        Success ast -> do
            let optimized = optimize ast
            let bashResult = compileToString optimized
            case bashResult of
                Left err ->
                    return $ traceShow optimized $ Left $ show err
                Right bash ->
                    return $ Right bash

compileBarlangSourceJs :: JSVal -> JSVal -> JSVal -> IO ()
compileBarlangSourceJs jsval resultCallback errorCallback = do
    let str = fromJSString jsval
    result <- compileBarlangSource str
    case result of
        Left err ->
            invokeCallback errorCallback (toJSString err)
        Right bash ->
            invokeCallback resultCallback (toJSString bash)

foreign import javascript unsafe "compileBarlang = $1"
    setCallback :: Callback a -> IO ()

foreign import javascript safe "$1($2);"
    invokeCallback :: JSVal -> JSVal -> IO ()

main :: IO ()
main = do
    callback <- syncCallback3 ContinueAsync compileBarlangSourceJs
    setCallback callback
