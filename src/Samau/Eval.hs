{-# LANGUAGE RankNTypes #-}

module Samau.Eval (evalTerm, evalExpr, execExpr) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict                       as M
import           Data.Maybe
import           Math.NumberTheory.ArithmeticFunctions
import           Math.NumberTheory.Primes.Counting
import           Math.NumberTheory.Primes.Sieve
import           Samau.Stack
import           Samau.Types

evalTerm :: SmTerm -> SmFunc
evalTerm (SmOp x) = fromMaybe (pure ()) $ M.lookup x builtins
evalTerm x        = push x

evalExpr :: SmExpr -> SmFunc
evalExpr = mapM_ evalTerm

execExpr :: SmExpr -> SmTerm
execExpr = flip execSm initStack . evalExpr

builtins :: M.Map Char SmFunc
builtins = M.fromList [
  -- 0x21, '!', pop
  ('!', void pop),
  -- 0x23, '#', length
  ('#', toSmFunc (length :: SmExpr -> Int)),
  -- 0x24, '$', swap
  ('$', dip pop >>= push),
  -- 0x26, '&', and
  ('&', toSmFuncList2 (&&)),
  -- 0x28, '(', pred
  ('(', toSmFuncEnum pred),
  -- 0x29, ')', succ
  (')', toSmFuncEnum succ),
  -- 0x2a, '*'
  ('*', toSmFuncNum2 (*)),
  -- 0x2b, '+'
  ('+', toSmFuncNum2 (+)),
  -- 0x2d, '-'
  ('-', toSmFuncNum2 (-)),
  -- 0x2e, '.', concat
  ('.', toSmFunc2 ((++) :: SmExpr -> SmExpr -> SmExpr)),
  -- 0x2f, '/'
  ('/', toSmFuncList2 ((/) :: Double -> Double -> Double)),
  -- 0x3a, ':', cons
  (':', toSmFunc2 ((:) :: SmTerm -> SmExpr -> SmExpr)),
  -- 0x3b, ';', dup
  (';', peek >>= push),
  -- 0x3c, '<'
  ('<', toSmFuncList2 ((<) :: Double -> Double -> Bool)),
  -- 0x3d, '=', same
  ('=', toSmFunc2 ((==) :: SmTerm -> SmTerm -> Bool)),
  -- 0x3e, '>'
  ('>', toSmFuncList2 ((>) :: Double -> Double -> Bool)),
  -- 0x3f, '?', if
  ('?', do
    x <- dip $ dip pop
    if fromSm x then void pop else void $ dip pop),
  -- 0x40, '@', roll
  ('@', dip (dip pop) >>= push),
  -- 0x4e, 'N', natural numbers
  ('N', push . SmList $ map SmInt [0..]),
  -- 0x50, 'P', primes
  ('P', push . SmList $ map SmInt primes),
  -- 0x5e, '^'
  ('^', smPower),
  -- 0x5f, '_'
  ('_', toSmFuncNum negate),
  -- 0x64, 'd', dip
  ('d', pop >>= dip . evalExpr . fromSm),
  -- 0x69, 'i'
  ('i', pop >>= evalExpr . fromSm),
  -- 0x6d, 'm', map
  ('m', do
    f <- pop
    xs <- pop
    mapSm (evalExpr $ fromSm f) (toList xs)),
  -- 0x7a, 'z', zipWith
  ('z', do
    f <- pop
    xs <- pop
    ys <- pop
    zipWithSm (evalExpr $ fromSm f) (toList xs) (toList ys)),
  -- 0x7c, '|', or
  ('|', toSmFuncList2 (||)),
  -- 0x7e, '~', not
  ('~', toSmFuncList not),
  -- 0x80, 'Α', abs
  ('Α', toSmFuncNum abs),
  -- 0xa7, 'π', prime counting
  ('π', toSmFuncList primeCount),
  -- 0xa9, 'σ', sigma
  ('σ', toSmFuncList2 (sigma :: Word -> Integer -> Integer)),
  -- 0xac, 'υ', n-th prime
  ('υ', toSmFuncList nthPrime),
  -- 0xad, 'φ', phi
  ('φ', toSmFuncList (totient :: Integer -> Integer))
  ]

applySm :: SmState a -> SmState SmTerm
applySm f = execSm f <$> stack

mapSm :: SmFunc -> [SmTerm] -> SmFunc
mapSm f xs = do
  s <- stack
  push . SmList $ map (execSm f . (:-: s)) xs

zipWithSm :: SmFunc -> [SmTerm] -> [SmTerm] -> SmFunc
zipWithSm f xs ys = do
  s <- stack
  push . SmList $ zipWith' (\x y -> execSm f (y :-: x :-: s)) xs ys

zipWith' :: (SmTerm -> SmTerm -> SmTerm) -> [SmTerm] -> [SmTerm] -> [SmTerm]
zipWith' _ [] []             = []
zipWith' f (x : xs) []       = f x SmNil : zipWith' f xs []
zipWith' f [] (y : ys)       = f SmNil y : zipWith' f [] ys
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

toSmFunc :: (SmType a, SmType b) => (a -> b) -> SmFunc
toSmFunc f = f . fromSm <$> pop >>= push . toSm

toSmFunc2 :: (SmType a, SmType b, SmType c) => (a -> b -> c) -> SmFunc
toSmFunc2 f = f . fromSm <$> pop <*> (fromSm <$> pop) >>= push . toSm

toSmFuncList :: (SmType a, SmType b) => (a -> b) -> SmFunc
toSmFuncList f = do
  x <- pop
  case x of
    SmList xs -> mapSm (toSmFuncList f) xs
    _         -> push . toSm . f $ fromSm x

toSmFuncList2 :: (SmType a, SmType b, SmType c) => (a -> b -> c) -> SmFunc
toSmFuncList2 f = do
  x <- pop
  y <- pop
  case (x, y) of
    (SmList xs, SmList ys) -> zipWithSm (toSmFuncList2 f) xs ys
    (SmList xs, _)         -> push y *> mapSm (toSmFuncList2 f) xs <* dip pop
    (_, SmList ys)         -> mapSm (push x *> toSmFuncList2 f) ys
    (_, _)                 -> push . toSm $ f (fromSm y) (fromSm x)

toSmFuncNum :: (forall a . Num a => a -> a) -> SmFunc
toSmFuncNum f = do
  x <- pop
  case x of
    SmList xs -> mapSm (toSmFuncNum f) xs
    SmFloat _ -> push (toSm . f $ (fromSm x :: Double))
    _         -> push (toSm . f $ (fromSm x :: Integer))

toSmFuncNum2 :: (forall a . Num a => a -> a -> a) -> SmFunc
toSmFuncNum2 f = do
  x <- pop
  y <- pop
  case (x, y) of
    (SmList xs, SmList ys) -> zipWithSm (toSmFuncNum2 f) xs ys
    (SmList xs, _)         -> push y *> mapSm (toSmFuncNum2 f) xs <* dip pop
    (_, SmList ys)         -> mapSm (push x *> toSmFuncNum2 f) ys
    (SmFloat _, _)         -> push (toSm $ f (fromSm y :: Double) (fromSm x :: Double))
    (_, SmFloat _)         -> push (toSm $ f (fromSm y :: Double) (fromSm x :: Double))
    (_, _)                 -> push (toSm $ f (fromSm y :: Integer) (fromSm x :: Integer))

toSmFuncEnum :: (forall a . Enum a => a -> a) -> SmFunc
toSmFuncEnum f = do
  x <- pop
  case x of
    SmList xs -> mapSm (toSmFuncEnum f) xs
    SmInt _   -> push (toSm . f $ (fromSm x :: Integer))
    SmFloat _ -> push (toSm . f $ (fromSm x :: Double))
    SmChar _  -> push (toSm . f $ (fromSm x :: Char))
    SmOp _    -> push (toSm . f $ (fromSm x :: Char))

toList :: SmTerm -> SmExpr
toList (SmInt x)   = map SmInt [0 .. x-1]
toList (SmFloat x) = map SmFloat [0 .. x-1]
toList (SmChar x)  = map SmChar [minBound .. pred x]
toList x           = fromSm x

-- 0x5e, '^'
smPower :: SmFunc
smPower = do
  x <- pop
  y <- pop
  case (x, y) of
    (SmList xs, SmList ys) -> zipWithSm smPower xs ys
    (SmList xs, _)         -> push y *> mapSm smPower xs <* dip pop
    (_, SmList ys)         -> mapSm (push x *> smPower) ys
    (SmFloat _, _)         -> push (toSm $ (fromSm y :: Double) ** (fromSm x :: Double))
    (_, SmFloat _)         -> push (toSm $ (fromSm y :: Double) ^ (fromSm x :: Integer))
    (_, _)                 -> push (toSm $ (fromSm y :: Integer) ^ (fromSm x :: Integer))
