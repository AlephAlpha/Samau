module Samau.Eval where

import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Samau.Stack
import           Samau.Types

evalTerm :: SmTerm -> SmFunc
evalTerm (SmOp x) = fromMaybe (pure ()) $ M.lookup x builtins
evalTerm x        = push x

evalExpr :: SmExpr -> SmFunc
evalExpr = mapM_ evalTerm

execExpr :: SmExpr -> SmTerm
execExpr = flip execSm initStack. evalExpr

builtins :: M.Map Char SmFunc
builtins = M.fromList [
  -- 0x21, '!', pop
  ('!', void pop),
  -- 0x24, '$', swap
  ('$', dip pop >>= push),
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
  -- 0x40, '@', roll
  ('@', dip (dip pop) >>= push),
  -- 0x5e, '^'
  ('^', smPower),
  -- 0x5f, '_'
  ('_', toSmFunNum negate),
  -- 0x64, 'd', dip
  ('d', pop >>= dip . evalExpr . fromSm),
  -- 0x69, 'i'
  ('i', pop >>= evalExpr . fromSm)
  ]

fromSm' :: SmTerm -> SmExpr
fromSm' (SmInt x)   = map SmInt [0 .. x-1]
fromSm' (SmFloat x) = map SmFloat [0 .. x-1]
fromSm' (SmChar x)  = map SmChar [minBound .. pred x]
fromSm' x           = fromSm x

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
