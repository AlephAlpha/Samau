module Main where

import Types
import Parser
import Eval
import CP737

import System.Console.Haskeline
import System.Environment

runProgram :: String -> String -> String
runProgram p = displayStack . evalString p . (:[]) . SmString

repl = do
 line <- getInputLine ":-) "
 case line of
   Nothing -> return ()
   Just p  -> outputStrLn (runProgram p []) >> repl

main = do
  args <- getArgs
  case args of
    []               -> runInputT defaultSettings repl
    ["--repl"]       -> runInputT defaultSettings repl
    [file]           -> readCP737File file >>= interact . runProgram
    ["--file", file] -> readCP737File file >>= interact . runProgram
    ["--utf8", file] -> readFile file >>= interact . runProgram
    ["--code", code] -> interact $ runProgram code
