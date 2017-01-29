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

stack :: SmState SmStack
stack = get

peek :: SmState SmTerm
peek = top <$> stack

dip :: SmState a -> SmState a
dip f = do
  x <- pop
  y <- f
  push x
  return y

execSm :: SmState a -> SmStack -> SmTerm
execSm f = top . execState f
