module Parser (smParse, readOneNumber, readNumbers) where

import Types

import Text.ParserCombinators.Parsec

parseInt :: Parser SmExpression
parseInt = many1 digit >>= return . SmInt . read

parseFloat :: Parser SmExpression
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many digit
  return $ SmFloat $ read $ x ++ "." ++ y ++ "0"

parseFloat' :: Parser SmExpression
parseFloat' = char '.' >> many1 digit >>= return . SmFloat . read . ("0." ++)

parseNegative :: Parser SmExpression
parseNegative = do
  char '_'
  x <- try parseFloat <|> try parseFloat' <|> try parseInt
  case x of
    SmInt y   -> return $ SmInt (-y)
    SmFloat y -> return $ SmFloat (-y)

parseChar :: Parser SmExpression
parseChar = do
  char '\''
  x <- try (noneOf "\\") <|>
       try (char '\\' >> oneOf "0abfntrv\"\'\\" >>= return . read . ("'\\" ++) . (: "'")) <|>
       char '\\'
  return $ SmChar x

parseString :: Parser SmExpression
parseString = do
  char '"'
  x <- many (try (noneOf "\\\"")
         <|> try (char '\\' >> oneOf "0abfntrv\"\'\\" >>= return . read . ("'\\" ++) . (: "'"))
         <|> char '\\')
  char '"'
  return $ SmString x

parseList :: Parser SmExpression
parseList = do
  char '['
  optional spaces
  x <- many parseExpression
  char ']'
  return $ SmList x

parseList' :: Parser SmExpression
parseList' = char '`' >> parseExpression >>= return . SmList . (:[])

parseEmpty :: Parser SmExpression
parseEmpty = char '`' >> spaces >> return (SmList [])

parseOperator :: Parser SmExpression
parseOperator = fmap SmOperator (noneOf " \"'1234567890[]`")

parseExpression :: Parser SmExpression
parseExpression = (parseString
               <|> parseList
               <|> try parseList'
               <|> parseEmpty
               <|> parseChar
               <|> try parseFloat
               <|> try parseFloat'
               <|> parseInt
               <|> try parseNegative
               <|> parseOperator) >>= ((optional spaces >>) . return)

parseProgram :: Parser SmProgram
parseProgram = optional spaces >> many parseExpression

runParse :: Parser a -> String -> a
runParse p s = case parse p "" s of
  Left _  -> error "Parse error"
  Right x -> x

smParse = runParse parseProgram

parseNumber :: Parser SmExpression
parseNumber = parseNegative <|> parseFloat' <|> try parseFloat <|> parseInt

parseOneNumber :: Parser SmExpression
parseOneNumber = many (noneOf "0123456789_.") >> (try parseNumber <|> (oneOf "_." >> parseOneNumber))

readOneNumber = runParse parseOneNumber

parseNumbers :: Parser SmExpression
parseNumbers = many (try parseOneNumber) >>= return . SmList

readNumbers = runParse parseNumbers
