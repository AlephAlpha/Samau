{-# LANGUAGE RankNTypes #-}

module Samau.Stack where

import           Control.Monad.State.Lazy
import           Samau.Types

data SmStack = SmTerm :-: SmStack
infixr 5 :-:

initStack :: SmStack
initStack = SmNil :-: initStack

top :: SmStack -> SmTerm
top (x :-: _) = x

type SmState = State SmStack

type SmFunc = SmState ()

push :: SmTerm -> SmFunc
push = modify . (:-:)

pop :: SmState SmTerm
pop = state (\(x :-: s) -> (x, s))

peek :: SmState SmTerm
peek = top <$> get

execSm :: SmState a -> SmStack -> SmTerm
execSm f = top . execState f

dip :: SmState a -> SmState a
dip f = do
  x <- pop
  y <- f
  push x
  return y

mapSm :: SmFunc -> [SmTerm] -> SmFunc
mapSm f xs = do
  s <- get
  push . SmList $ map (execSm f . (:-: s)) xs

zipWithSm :: SmFunc -> [SmTerm] -> [SmTerm] -> SmFunc
zipWithSm f xs ys = do
  s <- get
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

toSmFunNum :: (forall a . Num a => a -> a) -> SmFunc
toSmFunNum f = do
  x <- pop
  case x of
    SmList xs -> mapSm (toSmFunNum f) xs
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
