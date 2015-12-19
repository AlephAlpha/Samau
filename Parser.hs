module Parser (smParse) where

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
parseChar = char '\'' >> anyChar >>= return . SmChar

parseString :: Parser SmExpression
parseString = do
  char '"'
  x <- many (try (noneOf "\\\"") <|> (char '\\' >> anyChar >>= return . read . ("'\\" ++) . (: "'")))
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

parseOperator :: Parser SmExpression
parseOperator = fmap SmOperator (noneOf " \"'1234567890[]`")

parseExpression :: Parser SmExpression
parseExpression = (parseString
               <|> parseList
               <|> parseList'
               <|> parseChar
               <|> try parseFloat
               <|> try parseFloat'
               <|> parseInt
               <|> try parseNegative
               <|> parseOperator) >>= ((optional spaces >>) . return)

parseProgram :: Parser SmProgram
parseProgram = optional spaces >> many parseExpression

smParse x = case parse parseProgram "" x of
  Left _  -> error "Parse error"
  Right p -> p
