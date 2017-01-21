module Samau.Parser (readTerm, readExpr, readInt) where

import           Samau.Types
import           Text.Megaparsec
import           Text.Megaparsec.String

integer :: Num a => Parser a
integer = fromInteger . read <$> some digitChar

fractional :: Parser Double
fractional = char '.' *> (read . ("0." ++) <$> some digitChar)

float :: Parser Double
float = fractional <|> (+) <$> integer <*> (try fractional <|> char '.' *> return 0)

escape :: Parser Char
escape = try (read . ("'\\" ++) . (: "'") <$> (char '\\' >> oneOf "0abfntrv\"\'\\"))

parseNil :: Parser SmTerm
parseNil = string "` " *> pure SmNil

parseInt :: Parser SmTerm
parseInt = SmInt <$> (integer <|> char '_' *> (negate <$> integer))

parseFloat :: Parser SmTerm
parseFloat = SmFloat <$> (float <|> char '_' *> (negate <$> float))

parseChar :: Parser SmTerm
parseChar = SmChar <$> (char '\'' *> (escape <|> anyChar))

parseOp :: Parser SmTerm
parseOp = SmOp <$> noneOf " \"'1234567890[]`"

parseList :: Parser SmTerm
parseList = SmList <$> between (char '[') (char ']') parseExpr

parseString :: Parser SmTerm
parseString = SmList . map SmChar <$> between (char '"') (char '"') (many (escape <|> noneOf "\""))

parseQuote :: Parser SmTerm
parseQuote = SmList . (:[]) <$> (char '`' *> parseTerm)

parseTerm :: Parser SmTerm
parseTerm = (try parseNil <|> try parseFloat <|> try parseInt <|> parseChar <|> parseList <|> parseString <|> parseQuote <|> parseOp) <* skipMany (char ' ')

parseExpr :: Parser SmExpr
parseExpr = skipMany (char ' ') *> many parseTerm

readTerm :: String -> SmTerm
readTerm = either (error . parseErrorPretty) id . parse parseTerm ""

readExpr :: String -> SmExpr
readExpr = either (error . parseErrorPretty) id . parse parseExpr ""

readInt :: Num a => String -> Maybe a
readInt = parseMaybe $ integer <|> char '_' *> (negate <$> integer)
