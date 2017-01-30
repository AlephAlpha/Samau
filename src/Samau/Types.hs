{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Samau.Types where

data SmTerm = SmNil
            | SmInt Integer
            | SmFloat Double
            | SmChar Char
            | SmOp Char
            | SmList SmExpr
              deriving (Eq, Ord)

type SmExpr = [SmTerm]

isAtom :: SmTerm -> Bool
isAtom (SmList _) = False
isAtom _          = True

instance Show SmTerm where
  show SmNil       = " "
  show (SmInt x)   = (if x < 0 then "_" ++ show (-x) else show x) ++ " "
  show (SmFloat x) = (if x < 0 then "_" ++ show (-x) else show x) ++ " "
  show (SmChar x)  = ['\'', x]
  show (SmOp x)    = [x]
  show (SmList xs) = "[" ++ show xs ++ "]"
  showList = (++) . concatMap show

class SmType a where
  fromSm :: SmTerm -> a
  toSm :: a -> SmTerm

instance SmType SmTerm where
  fromSm = id
  toSm = id

instance SmType Integer where
  fromSm SmNil       = 0
  fromSm (SmInt x)   = x
  fromSm (SmFloat x) = floor x
  fromSm (SmChar x)  = toInteger $ fromEnum x
  fromSm (SmOp x)    = toInteger $ fromEnum x
  toSm = SmInt

instance SmType Int where
  fromSm = fromInteger . fromSm
  toSm = SmInt . toInteger

instance SmType Word where
  fromSm = fromInteger . fromSm
  toSm = SmInt . toInteger

instance SmType Double where
  fromSm (SmFloat x) = x
  fromSm s           = fromInteger $ fromSm s
  toSm = SmFloat

instance SmType Char where
  fromSm SmNil       = ' '
  fromSm (SmInt x)   = toEnum $ fromInteger x
  fromSm (SmFloat x) = toEnum $ floor x
  fromSm (SmChar x)  = x
  fromSm (SmOp x)    = x
  toSm = SmChar

instance SmType SmExpr where
  fromSm SmNil       = []
  fromSm (SmList xs) = xs
  fromSm x           = [x]
  toSm = SmList

instance SmType String where
  fromSm SmNil       = ""
  fromSm (SmInt x)   = show x
  fromSm (SmFloat x) = show x
  fromSm (SmChar x)  = [x]
  fromSm (SmOp x)    = [x]
  fromSm (SmList xs) = xs >>= fromSm
  toSm = SmList . map SmChar

instance SmType Bool where
  fromSm SmNil       = False
  fromSm (SmInt x)   = x == 0
  fromSm (SmFloat x) = x == 0
  fromSm (SmChar x)  = x == ' '
  fromSm (SmOp x)    = True
  fromSm (SmList xs) = null xs
  toSm = SmInt . toInteger . fromEnum

instance SmType () where
  fromSm = const ()
  toSm = const SmNil
