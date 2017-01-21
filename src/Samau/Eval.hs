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
  -- 0x2a, '*'
  ('*', toSmFuncNum2 (*)),
  -- 0x2b, '+'
  ('+', toSmFuncNum2 (+)),
  -- 0x2d, '-'
  ('-', toSmFuncNum2 (-)),
  -- 0x2f, '/'
  ('/', toSmFuncList2 ((/) :: Double -> Double -> Double)),
  -- 0x3a, ':', cons
  (':', toSmFunc2 ((:) :: SmTerm -> SmExpr -> SmExpr)),
  -- 0x3b, ';', dup
  (';', peek >>= push),
  -- 0x64, 'd', dip
  ('d', pop >>= dip . evalExpr . fromSm)
  ]
