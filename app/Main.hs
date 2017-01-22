module Main where

import           Options.Applicative
import           Samau.Repl

data Options = Repl | File FilePath

parseOpts :: Parser Options
parseOpts = (File <$> strArgument (metavar "FILENAME")) <|> pure Repl

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Repl      -> runRepl
    File file -> readFile file >>= putStrLn . rep
