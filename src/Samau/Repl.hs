module Samau.Repl where

import           Samau.Eval
import           Samau.Parser
import           Samau.Types
import           System.Console.Haskeline

execExpr' :: SmExpr -> SmExpr
execExpr' = fromSm . execExpr

rep :: String -> String
rep = show . execExpr' . readExpr

repl :: InputT IO ()
repl = do
  m <- getInputLine ":-) "
  case m of
    Nothing -> return ()
    Just "" -> repl
    Just s  -> do
      catch (outputStrLn $ rep s) $ \e -> outputStrLn $ show (e :: SomeException)
      repl

runRepl :: IO ()
runRepl = runInputT defaultSettings repl
