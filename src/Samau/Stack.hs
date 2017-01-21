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

mapSm :: SmFunc -> SmFunc
mapSm f = do
  xs <- pop
  s <- get
  push . SmList . map (execSm f . (:-: s)) $ fromSm' xs

zipWithSm :: SmFunc -> SmFunc
zipWithSm f = do
  xs <- pop
  ys <- pop
  s <- get
  push . SmList $ zipWith' (\x y -> execSm f (y :-: x :-: s)) (fromSm' xs) (fromSm' ys)

fromSm' :: SmTerm -> SmExpr
fromSm' (SmInt x)   = map SmInt [0..x-1]
fromSm' (SmFloat x) = map SmFloat [0..x-1]
fromSm' x           = fromSm x

zipWith' :: (SmTerm -> SmTerm -> SmTerm) -> [SmTerm] -> [SmTerm] -> [SmTerm]
zipWith' _ [] []             = []
zipWith' f (x : xs) []       = f x SmNil : zipWith' f xs []
zipWith' f [] (y : ys)       = f SmNil y : zipWith' f [] ys
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

toSmFunc :: (SmType a, SmType b) => (a -> b) -> SmFunc
toSmFunc f = do
  x <- pop
  push . toSm . f $ fromSm x

toSmFunc2 :: (SmType a, SmType b, SmType c) => (a -> b -> c) -> SmFunc
toSmFunc2 f = do
  x <- pop
  y <- pop
  push . toSm $ f (fromSm y) (fromSm x)

toSmFunc' :: (SmType a, SmType b) => (a -> b) -> SmFunc
toSmFunc' f = do
  x <- peek
  case x of
    SmList _ -> mapSm $ toSmFunc' f
    _        -> toSmFunc f

toSmFunc2' :: (SmType a, SmType b, SmType c) => (a -> b -> c) -> SmFunc
toSmFunc2' f = do
  x <- peek
  y <- dip peek
  case (x, y) of
    (SmList _, SmList _) -> zipWithSm $ toSmFunc2' f
    (SmList _, _)        -> mapSm $ toSmFunc2 f <* dip pop
    (_, SmList _)        -> pop *> mapSm (push x *> toSmFunc2 f)
    (_, _)               -> toSmFunc2 f
