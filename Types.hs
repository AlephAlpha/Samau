module Types where

data SmExpression = SmInt Integer
                  | SmFloat Double
                  | SmChar Char
                  | SmString String
                  | SmList [SmExpression]
                  | SmOperator Char
                    deriving (Show,Eq,Read,Ord)

type SmProgram = [SmExpression]
type SmStack = [SmExpression]
type SmFunction = SmStack -> SmStack

-- Type Conversion
toInt :: SmExpression -> Integer
toInt (SmInt x)      = x
toInt (SmFloat x)
  | x >= 0           = floor x
  | otherwise        = ceiling x
toInt (SmChar x)     = toInteger $ fromEnum x
toInt (SmOperator x) = toInteger $ fromEnum x

toFloat :: SmExpression -> Double
toFloat (SmInt x)       = fromInteger x
toFloat (SmFloat x)     = x
toFloat (SmChar x)      = fromIntegral $ fromEnum x
toFloat (SmOperator x)  = fromIntegral $ fromEnum x

toChar :: SmExpression -> Char
toChar (SmInt x)       = toEnum $ fromIntegral x
toChar (SmFloat x)     = toEnum $ floor x
toChar (SmChar x)      = x
toChar (SmOperator x)  = x

toString :: SmExpression -> String
toString (SmInt x)      = show x
toString (SmFloat x)    = show x
toString (SmChar x)     = [x] 
toString (SmString xs)  = xs
toString (SmList xs)    = xs >>= toString
toString (SmOperator x) = [x]

toList :: SmExpression -> [SmExpression]
toList (SmString xs) = map SmChar xs
toList (SmList xs)   = xs
toList x             = [x]
