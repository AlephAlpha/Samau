module Types where

data SmExpression = SmInt Integer
                  | SmFloat Double
                  | SmChar Char
                  | SmOperator Char
                  | SmString String
                  | SmList [SmExpression]
                    deriving (Show,Eq,Read)

instance Ord SmExpression where
  (SmInt x1) <= (SmInt x2)           = x1 <= x2
  (SmFloat x1) <= (SmFloat x2)       = x1 <= x2
  (SmChar x1) <= (SmChar x2)         = x1 <= x2
  (SmOperator x1) <= (SmOperator x2) = x1 <= x2
  (SmString x1) <= (SmString x2)     = x1 <= x2
  (SmList x1) <= (SmList x2)         = x1 <= x2
  (SmInt x1) <= (SmFloat x2)         = fromInteger x1 <= x2
  (SmFloat x1) <= (SmInt x2)         = x1 < fromInteger x2
  (SmInt _) <= _                     = True
  (SmFloat _) <= _                   = True
  (SmChar _) <= (SmInt _)            = False
  (SmChar _) <= (SmFloat _)          = False
  (SmChar _) <= _                    = True
  (SmOperator _) <= (SmString _)     = True
  (SmOperator _) <= (SmList _)       = True
  (SmString _) <= (SmList _)         = True
  _ <= _                             = False

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
