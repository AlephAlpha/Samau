module Eval (evalProgram, evalExpression, evalString, displayStack, builtins) where

import Types
import Parser

import Data.Char
import Data.List
import qualified Data.Map as M

evalProgram :: SmProgram -> SmFunction
evalProgram = flip $ foldl $ flip evalExpression

evalExpression :: SmExpression -> SmFunction
evalExpression (SmOperator o) = case M.lookup o builtins of
  Just f  -> f
  Nothing -> id
evalExpression x              = (x:)

evalString :: String -> SmFunction
evalString = evalProgram . smParse

-- Some helper functions

isTruthy :: SmExpression -> Bool
isTruthy (SmInt 0)     = False
isTruthy (SmFloat 0)   = False
isTruthy (SmString "") = False
isTruthy (SmList [])   = False
isTruthy _             = True

isFalsy :: SmExpression -> Bool
isFalsy = not . isTruthy

isAtom :: SmExpression -> Bool
isAtom (SmInt _)      = True
isAtom (SmFloat _)    = True
isAtom (SmChar _)     = True
isAtom (SmOperator _) = True
isAtom _              = False

fromBool :: Bool -> SmExpression
fromBool True  = SmInt 1
fromBool False = SmInt 0

evalIfList :: SmExpression -> SmFunction
evalIfList (SmList q)   = evalProgram q
evalIfList (SmString q) = evalString q
evalIfList x            = evalExpression x

evalIfList1 :: SmExpression -> SmStack -> SmExpression
evalIfList1 x s = case evalIfList x s of
  y:_ -> y
  []  -> SmInt 0

displayExpression :: SmExpression -> String
displayExpression (SmInt x)
  | x < 0                        = "_" ++ show (-x) ++ " "
  | otherwise                    = show x ++ " "
displayExpression (SmFloat x)
  | x < 0                        = "_" ++ show (-x) ++ " "
  | otherwise                    = show x ++ " "
displayExpression (SmChar x)     = '\'':[x]
displayExpression (SmString xs)  = show xs
displayExpression (SmList xs)    = "[" ++ (xs >>= displayExpression) ++ "]"
displayExpression (SmOperator x) = [x]

displayStack :: SmStack -> String
displayStack (SmString xs:_) = xs
displayStack (SmList xs:_)   = xs >>= displayExpression
displayStack (SmChar x:_)    = [x]
displayStack (x:_)           = displayExpression x
displayStack []              = ""

toListFunction :: (SmStack -> SmExpression) -> SmFunction
toListFunction f (xs:s)   = SmList (map (f . (:s)) $ toList xs):s

toListFunction2 :: (SmStack -> SmExpression) -> SmFunction
toListFunction2 f (xs1:xs2:s) = SmList (zipWith (\x1 x2 -> f (x1:x2:s)) (toList xs1) (toList xs2)):s

builtins = M.fromList [('!', smPop),
                       ('#', smSize),
                       ('$', smSwap),
                       ('%', smMod),
                       ('&', smAnd),
                       ('(', smPred),
                       (')', smSucc),
                       ('*', smTimes),
                       ('+', smAdd),
                       (',', smRange0),
                       ('-', smMinus),
                       ('.', smJoin),
                       ('/', smDivide),
                       (':', smCons),
                       (';', smDup),
                       ('<', smLess),
                       ('=', smSame),
                       ('>', smGreater),
                       ('?', smIf),
                       ('@', smRoll),
                       ('A', smAnswer),
                       ('E', smE),
                       ('N', smNaturals),
                       ('O', smPi),
                       ('\\', smUncons),
                       ('^', smPower),
                       ('_', smNegative),
                       ('c', smTake),
                       ('d', smDip),
                       ('e', smDrop),
                       ('f', smFold),
                       ('g', smFold1),
                       ('i', smI),
                       ('m', smMap),
                       ('o', smOuter),
                       ('s', smFilter),
                       ('t', smTwice),
                       ('x', smX),
                       ('w', smNest),
                       ('y', smFixedPoint),
                       ('z', smZipWith),
                       ('{', smUnstack),
                       ('|', smOr),
                       ('}', smStack),
                       ('~', smNot),
                       ('Α', smAbs),
                       ('Δ', smDiff),
                       ('Ε', smExp),
                       ('Π', smProduct),
                       ('Σ', smSum),
                       ('░', smToInt),
                       ('▒', smToFloat),
                       ('▓', smToChar),
                       ('│', smDivisible),
                       ('╡', smIndex),
                       ('╢', smElem),
                       ('╖', smTail),
                       ('╕', smHead),
                       ('╣', smSubsets),
                       ('║', smReverse),
                       ('╗', smTails),
                       ('╝', smIntersperse),
                       ('╜', smSort),
                       ('╛', smNub),
                       ('└', smFloor),
                       ('─', smRound),
                       ('╟', smPosition),
                       ('╚', smConcat),
                       ('╔', smInits),
                       ('╠', smPermutations),
                       ('═', smEq),
                       ('╧', smCycle),
                       ('╒', smLast),
                       ('╓', smInit),
                       ('┌', smCeiling),
                       ('█', smToString),
                       ('▄', smToList),
                       ('▌', smReadOneNumber),
                       ('▐', smReadNumbers),
                       ('▀', smReadProgram),
                       ('±', smSign),
                       ('≥', smGreaterEq),
                       ('≤', smLessEq),
                       ('÷', smDiv),
                       ('√', smSqrt)]

-- Built-in functions, sorted by names

-- SmOperator 'Α'
smAbs (SmInt x:s)   = SmInt (abs x):s
smAbs (SmFloat x:s) = SmFloat (abs x):s
smAbs (x:s)
  | isAtom x        = SmInt (abs $ toInt x):s
  | otherwise       = toListFunction (head . smAbs) (x:s)
smAbs s             = s

-- SmOperator '+'
smAdd (SmInt x1:SmInt x2:s)     = SmInt (x1 + x2):s
smAdd (SmInt x1:SmFloat x2:s)   = SmFloat (fromInteger x1 + x2):s
smAdd (SmFloat x1:SmInt x2:s)   = SmFloat (x1 + fromInteger x2):s
smAdd (SmFloat x1:SmFloat x2:s) = SmFloat (x1 + x2):s
smAdd (SmChar x1:x2:s)          = smAdd $ smToInt (SmChar x1:x2:s)
smAdd (x1:SmChar x2:s)          = smAdd (SmChar x2:x1:s)
smAdd (x1:x2:s)
  | isAtom x1 && isAtom x2      = smAdd $ smToInt $ x1:smToInt (x2:s)
  | isAtom x2                   = toListFunction (head . smAdd) (x1:x2:s)
  | isAtom x1                   = smAdd (x2:x1:s)
  | otherwise                   = toListFunction2 (head . smAdd) (x1:x2:s)
smAdd []                        = [SmInt 0]
smAdd s                         = s

-- SmOperator '&'
smAnd (x1:x2:s) = fromBool (isTruthy x1 && isTruthy x2):s
smAnd []        = [SmInt 1]
smAnd s         = s

-- SmOperator 'A'
smAnswer s = SmInt 42:s

-- SmOperator '┌'
smCeiling (SmInt x:s)    = SmInt x:s
smCeiling (SmFloat x:s)  = SmInt (floor x):s
smCeiling (SmChar x:s)   = SmChar (toLower x):s
smCeiling (SmList x:s)   = toListFunction (head . smCeiling) (SmList x:s)
smCeiling (SmString x:s) = smToString $ toListFunction (head . smCeiling) (SmString x:s)
smCeiling s              = s

-- SmOperator '╚'
smConcat (x:s)
  | isAtom x  = x:s
  | otherwise = smFold (SmOperator '.':x:SmList []:s)
smConcat s    = [SmList []]

-- SmOperator ':'
smCons (SmList xs:x:s)   = SmList (x:xs):s
smCons (SmString xs:x:s) = SmString (toString x ++ xs):s
smCons (SmChar x1:x2:s)  = SmString (toString x2 ++ [x1]):s
smCons (x1:x2:s)         = SmList [x2,x1]:s
smCons s                 = [SmList s]

-- SmOperator '╧'
smCycle (SmChar x:s)    = SmString (repeat x):s
smCycle (SmString xs:s) = SmString (cycle xs):s
smCycle (SmList xs:s)   = SmList (cycle xs):s
smCycle (x:s)           = SmList (repeat x):s
smCycle s               = s

-- SmOperator 'Δ'
smDiff (SmList []:s) = SmList []:s
smDiff (SmList xs:s) = smMinus (SmList xs:SmList (tail xs):s)
smDiff s             = smDiff $ smToList s

-- SmOperator 'd'
smDip (q:x:s) = x:evalIfList q s
smDip s       = s

-- SmOperator '÷'
smDiv (SmInt x1:SmInt x2:s)     = SmInt (div x2 x1):s
smDiv (SmInt x1:SmFloat x2:s)   = smFloor $ smDivide $SmInt x1:SmFloat x2:s
smDiv (SmFloat x1:SmInt x2:s)   = smFloor $ smDivide $SmFloat x1:SmInt x2:s
smDiv (SmFloat x1:SmFloat x2:s) = smFloor $ smDivide $SmFloat x1:SmFloat x2:s
smDiv (SmChar x1:x2:s)          = smDiv $ smToInt (SmChar x1:x2:s)
smDiv (x1:SmChar x2:s)          = smDiv $ x1:smToInt (SmChar x2:s)
smDiv (x1:x2:s)
  | isAtom x1 && isAtom x2        = smDiv $ smToInt $ x1:smToInt (x2:s)
  | isAtom x2                     = toListFunction (head . smDiv) (x1:x2:s)
  | isAtom x1                     = toListFunction (head . smDiv . (x1:)) (x2:s)
  | otherwise                     = toListFunction2 (head . smDiv) (x1:x2:s)

-- SmOperator '/'
smDivide (x1:x2:s)
  | isAtom x1 && isAtom x2 = SmFloat (toFloat x2 / toFloat x1):s
  | isAtom x2              = toListFunction (head . smDivide) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smDivide . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smDivide) (x1:x2:s)
smDivide []                = [SmFloat 1]
smDivide s                 = smToFloat s

-- SmOperator '│'
smDivisible (x1:x2:s)
  | isAtom x1 && isAtom x2 = smNot $ smMod (x1:x2:s)
  | isAtom x2              = toListFunction (head . smDivisible) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smDivisible . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smDivisible) (x1:x2:s)
smDivisible s              = s

-- SmOperator 'e'
smDrop (x:SmList xs:s)
  | isAtom x             = SmList (drop (fromInteger $ toInt x) xs):s
  | otherwise            = SmList (dropWhile (isTruthy . evalIfList1 x . (:s)) xs):s
smDrop (x:SmString xs:s) = smDrop $ x:smToList (SmString xs:s)
smDrop s                 = s

-- SmOperator ';'
smDup (x:s) = x:x:s
smDup s     = s

-- SmOperator 'E'
smE s = SmFloat (exp 1):s

-- SmOperator 'Ε'
smExp (x:s)
  | isAtom x  = SmFloat (exp $ toFloat x):s
  | otherwise = toListFunction (head . smExp) (x:s)
smExp s       = s

-- SmOperator '╢'
smElem (x:SmList xs:s)   = fromBool (elem x xs):s
smElem (x:SmString xs:s) = fromBool (elem (toChar x) xs):s
smElem s                 = s

-- SmOperator '═'
smEq (x1:x2:s)
  | isAtom x1 && isAtom x2 = fromBool (toFloat x2 == toFloat x1):s
  | isAtom x2              = toListFunction (head . smEq) (x1:x2:s)
  | isAtom x1              = smEq (x2:x1:s)
  | otherwise              = toListFunction2 (head . smEq) (x1:x2:s)
smEq s                     = s

-- SmOperator 's'
smFilter (q:SmList xs:s)   = SmList (filter (isTruthy . evalIfList1 q . (:s)) xs):s
smFilter (q:SmString xs:s) = SmString (filter (isTruthy . evalIfList1 q . (:s) . SmChar) xs):s
smFilter s                 = s

-- SmOperator 'y'
smFixedPoint (q:s)
  | evalIfList q s == s = s
  | otherwise           = smFixedPoint (q:evalIfList q s)
smNsmFixedPointest s    = s

-- SmOperator '└'
smFloor (SmInt x:s)    = SmInt x:s
smFloor (SmFloat x:s)  = SmInt (floor x):s
smFloor (SmChar x:s)   = SmChar (toLower x):s
smFloor (SmList x:s)   = toListFunction (head . smFloor) (SmList x:s)
smFloor (SmString x:s) = smToString $ toListFunction (head . smFloor) (SmString x:s)
smFloor s              = s

-- SmOperator 'f'
smFold (q:SmList []:s)     = s
smFold (q:SmList (x:xs):s) = smFold (q:SmList xs:evalIfList q (x:s))
smFold (q:SmString xs:s)   = smFold (q:SmList (toList $ SmString xs):s)
smFold (q:x:s)             = smFold (q:smRange0 (x:s))
smFold s                   = s

-- SmOperator 'g'
smFold1 (q:x:s)
  | isAtom x  = smFold1 (q:smRange0 (x:s))
  | otherwise = smFold $ q:smUncons (x:s)

-- SmOperator '>'
smGreater (x1:x2:s)
  | isAtom x1 && isAtom x2 = fromBool (toFloat x2 > toFloat x1):s
  | isAtom x2              = toListFunction (head . smGreater) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smGreater . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smGreater) (x1:x2:s)
smGreater s                = s

-- SmOperator '≥'
smGreaterEq (x1:x2:s)
  | isAtom x1 && isAtom x2 = fromBool (toFloat x2 >= toFloat x1):s
  | isAtom x2              = toListFunction (head . smGreaterEq) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smGreaterEq . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smGreaterEq) (x1:x2:s)
smGreaterEq s              = s

-- SmOperator '╕'
smHead (SmList (x:xs):s)   = x:s
smHead (SmList []:s)       = s
smHead (SmString (x:xs):s) = SmChar x:s
smHead (SmString []:s)     = s
smHead s                   = s

-- SmOperator 'i'
smI (q:s) = evalIfList q s
smI s     = s

-- SmOperator '?'
smIf (q1:q2:t:s) = case evalIfList t s of
  u:_ | isTruthy u -> evalIfList q2 s
      | otherwise  -> evalIfList q1 s
  _                -> evalIfList q1 s
smIf s           = s

-- SmOperator '╡'
smIndex (x:SmList xs:s)
  | isAtom x  = genericIndex (cycle xs) (toInt x):s
  | otherwise = toListFunction (head . smIndex) (x:SmList xs:s)
smIndex (x:SmString xs:s)
  | isAtom x  = SmChar (genericIndex (cycle xs) (toInt x)):s
  | otherwise = smToString $ toListFunction (head . smIndex) (x:SmString xs:s)
smIndex s     = s

-- SmOperator '╓'
smInit (SmList (x:xs):s)   = SmList (init $ x:xs):s
smInit (SmList []:s)       = s
smInit (SmString (x:xs):s) = SmString (init $ x:xs):s
smInit (SmString []:s)     = s
smInit s                   = s

-- SmOperator '╔'
smInits (SmList xs:s)   = SmList (map SmList $ inits xs):s
smInits (SmString xs:s) = SmList (map SmString $ inits xs):s
smInits s               = s

-- SmOperator '╝'
smIntersperse (x:SmList xs:s)   = SmList (intersperse x xs):s
smIntersperse (x:SmString xs:s) = SmString (intersperse (toChar x) xs):s
smIntersperse s                 = s

-- SmOperator '.'
smJoin (SmList xs1:SmList xs2:s)     = SmList (xs2 ++ xs1):s
smJoin (SmString xs1:SmString xs2:s) = SmString (xs2 ++ xs1):s
smJoin (x:SmString xs:s)             = SmString (xs ++ toString x):s
smJoin (SmString xs:x:s)             = SmString (toString x ++ xs):s
smJoin (x:SmList xs:s)               = SmList (xs ++ toList x):s
smJoin (x1:x2:s)                     = smJoin (x1:SmList [x2]:s)
smJoin [SmList xs]                   = [SmList xs]
smJoin s                             = [SmList s]

-- SmOperator '╒'
smLast (SmList (x:xs):s)   = (last $ x:xs):s
smLast (SmList []:s)       = s
smLast (SmString (x:xs):s) = SmChar (last $ x:xs):s
smLast (SmString []:s)     = s
smLast s                   = s

-- SmOperator '<'
smLess (x1:x2:s)
  | isAtom x1 && isAtom x2 = fromBool (toFloat x2 < toFloat x1):s
  | isAtom x2              = toListFunction (head . smLess) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smLess . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smLess) (x1:x2:s)
smLess s                   = s

-- SmOperator '≤'
smLessEq (x1:x2:s)
  | isAtom x1 && isAtom x2 = fromBool (toFloat x2 <= toFloat x1):s
  | isAtom x2              = toListFunction (head . smLessEq) (x1:x2:s)
  | isAtom x1              = toListFunction (head . smLessEq . (x1:)) (x2:s)
  | otherwise              = toListFunction2 (head . smLessEq) (x1:x2:s)
smLessEq s                 = s

-- SmOperator 'm'
smMap (q:x:s)
  | isAtom x  = smMap (q:smRange0 (x:s))
  | otherwise = toListFunction (evalIfList1 q) (x:s)
smMap s       = s

-- SmOperator '-'
smMinus s = smAdd $ smNegative s

-- SmOperator '%'
smMod (x1:x2:s) = smMinus (y1:x2:s) where
  y1:_ = smTimes $ y2:x1:s
  y2:_ = smDiv $ x1:x2:s
smMod s         = s

-- SmOperator 'N'
smNaturals s = SmList (map SmInt [0..]):s

-- SmOperator '_'
smNegative (SmInt x:s)   = SmInt (-x):s
smNegative (SmFloat x:s) = SmFloat (-x):s
smNegative (SmChar x:s)  = SmInt (- (toInt $ SmChar x)):s
smNegative (x:s)
  | isAtom x             = x:s
  | otherwise            = toListFunction (head . smNegative) (x:s)
smNegative s             = s

-- SmOperator 'w'
smNest (q:t:s)
  | isAtom t  = if toInt t <= 0 then s else smNest $ q:smPred (t:evalIfList q s)
  | otherwise = case evalIfList t s of
    u:_ | isTruthy u -> smNest (q:t:evalIfList q s)
        | otherwise  -> s
    _                -> []
smNest s      = s

-- SmOperator '~'
smNot (x:s) = fromBool (isFalsy x):s
smNot s       = [SmInt 0]

-- SmOperator '╛'
smNub (SmList xs:s)   = SmList (nub xs):s
smNub (SmString xs:s) = SmString (nub xs):s
smNub s               = s

-- SmOperator '|'
smOr (x1:x2:s) = fromBool (isTruthy x1 || isTruthy x2):s
smOr []        = [SmInt 0]
smOr s         = s

-- SmOperator 'o'
smOuter (q:x1:x2:s)
  | isAtom x1 = smOuter (q:smRange0 (x1:x2:s))
  | isAtom x2 = smOuter (q:x1:smRange0 (x2:s))
  | otherwise = toListFunction (head . toListFunction (evalIfList1 q) . (x1:)) (x2:s)
smOuter s   = s 

-- SmOperator '╠'
smPermutations (SmList xs:s)   = SmList (map SmList $permutations xs):s
smPermutations (SmString xs:s) = SmList (map SmString $ permutations xs):s
smPermutations (x:s)           = smPermutations $ smRange0 (x:s)
smPermutations s               = s

-- SmOperator 'O'
smPi s = SmFloat pi:s

-- SmOperator '!'
smPop (x:s) = s
smPop s     = s

-- SmOperator '╟'
smPosition (x:SmList xs:s)   = SmList (map (SmInt . fromIntegral) $ elemIndices x xs):s
smPosition (x:SmString xs:s) = SmList (map (SmInt . fromIntegral) $ elemIndices (toChar x) xs):s
smPosition s                 = s

-- SmOperator '^'
smPower (SmInt x1:SmInt x2:s)     = SmInt (x2 ^ x1):s
smPower (SmInt x1:SmFloat x2:s)   = SmFloat (x2 ^ x1):s
smPower (SmFloat x1:SmInt x2:s)   = SmFloat (fromInteger x2 ** x1):s
smPower (SmFloat x1:SmFloat x2:s) = SmFloat (x2 ** x1):s
smPower (SmChar x1:x2:s)          = smPower $ smToInt (SmChar x1:x2:s)
smPower (x1:SmChar x2:s)          = smPower $ x1:smToInt (SmChar x2:s)
smPower (x1:x2:s)
  | isAtom x1 && isAtom x2        = smPower $ smToInt $ x1:smToInt (x2:s)
  | isAtom x2                     = toListFunction (head . smPower) (x1:x2:s)
  | isAtom x1                     = toListFunction (head . smPower . (x1:)) (x2:s)
  | otherwise                     = toListFunction2 (head . smPower) (x1:x2:s)
smPower s                         = s

-- SmOperator '('
smPred s = smMinus (SmInt 1:s)

-- SmOperator 'Π'
smProduct (x:s)
  | isAtom x  = smProduct $ smUncons $ x:s
  | otherwise = smFold (SmOperator '*':x:SmInt 1:s)
smProduct s   = [SmInt 1]

-- SmOperator ','
smRange0 (x:s)
  | isAtom x  = let y = toInt x in SmList (map SmInt $ if y>=0 then [0..y-1] else [-y-1,-y-2..0]):s
  | otherwise = smRange0 $ smSize (x:s)
smRange0 s    = [SmList []]

-- SmOperator '▐'
smReadNumbers (SmString x:s) = readNumbers x:s
smReadNumbers s              = s

-- SmOperator '▌'
smReadOneNumber (SmString x:s) = readOneNumber x:s
smReadOneNumber s              = s

-- SmOperator '▀'
smReadProgram (SmString x:s) = evalString x s 
smReadProgram s              = smI s

-- SmOperator '║'
smReverse (SmList xs:s)   = SmList (reverse xs):s
smReverse (SmString xs:s) = SmString (reverse xs):s
smReverse s               = s

-- SmOperator '@'
smRoll (x1:x2:x3:s) = x3:x1:x2:s
smRoll s            = s

-- SmOperator '─'
smRound (SmInt x:s)    = SmInt x:s
smRound (SmFloat x:s)  = SmInt (round x):s
smRound (SmList x:s)   = toListFunction (head . smRound) (SmList x:s)
smRound s              = s

-- SmOperator '='
smSame (x1:x2:s)
  | x1 == x2  = SmInt 1:s
  | otherwise = SmInt 0:s
smSame s      = [SmInt 0]

-- SmOperator '±'
smSign (x:s)
  | isAtom x  = case compare (toFloat x) 0 of
    GT -> SmInt 1:s
    EQ -> SmInt 0:s
    LT -> SmInt (-1):s
  | otherwise = toListFunction (head . smSign) (x:s)
smSign s      = s

-- SmOperator '#'
smSize (SmList xs:s)   = SmInt (genericLength xs):s
smSize (SmString xs:s) = SmInt (genericLength xs):s
smSize (x:s)           = SmInt 1:s
smSize s               = SmInt 0:s

-- SmOperator '╜'
smSort (SmList xs:s)   = SmList (sort xs):s
smSort (SmString xs:s) = SmString (sort xs):s
smSort s               = s

-- SmOperator '√'
smSqrt s = smPower (SmFloat 0.5:s)

-- SmOperator '}'
smStack s = SmList s:s

-- SmOperator ')'
smSucc s = smAdd (SmInt 1:s)

-- SmOperator '╣'
smSubsets (SmList xs:s)   = SmList (map SmList $subsequences xs):s
smSubsets (SmString xs:s) = SmList (map SmString $ subsequences xs):s
smSubsets (x:s)           = smSubsets $ smRange0 (x:s)
smSubsets s               = s

-- SmOperator 'Σ'
smSum (x:s)
  | isAtom x  = smSum $ smUncons $ x:s
  | otherwise = smFold (SmOperator '+':x:SmInt 0:s)
smSum s       = [SmInt 0]

-- SmOperator '$'
smSwap (x1:x2:s) = x2:x1:s
smSwap s         = s

-- SmOperator '╖'
smTail (SmList (x:xs):s)   = SmList xs:s
smTail (SmList []:s)       = s
smTail (SmString (x:xs):s) = SmString xs:s
smTail (SmString []:s)     = s
smTail s                   = s

-- SmOperator '╗'
smTails (SmList xs:s)   = SmList (map SmList $ tails xs):s
smTails (SmString xs:s) = SmList (map SmString $ tails xs):s
smTails s               = s

-- SmOperator 'c'
smTake (x:SmList xs:s)
  | isAtom x             = SmList (take (fromInteger $ toInt x) xs):s
  | otherwise            = SmList (takeWhile (isTruthy . evalIfList1 x . (:s)) xs):s
smTake (x:SmString xs:s) = smTake $ x:smToList (SmString xs:s)
smTake s                 = s

-- SmOperator '*'
smTimes (SmInt x1:SmInt x2:s)     = SmInt (x1 * x2):s
smTimes (SmInt x1:SmFloat x2:s)   = SmFloat (fromInteger x1 * x2):s
smTimes (SmFloat x1:SmInt x2:s)   = SmFloat (x1 * fromInteger x2):s
smTimes (SmFloat x1:SmFloat x2:s) = SmFloat (x1 * x2):s
smTimes (SmChar x1:x2:s)          = smTimes $ smToInt (SmChar x1:x2:s)
smTimes (x1:SmChar x2:s)          = smTimes (SmChar x2:x1:s)
smTimes (x1:x2:s)
  | isAtom x1 && isAtom x2        = smTimes $ smToInt $ x1:smToInt (x2:s)
  | isAtom x2                     = toListFunction (head . smTimes) (x1:x2:s)
  | isAtom x1                     = smTimes (x2:x1:s)
  | otherwise                     = toListFunction2 (head . smTimes) (x1:x2:s)
smTimes []                        = [SmInt 1]
smTimes s                         = s

-- SmOperator '▓'
smToChar (x:s)
  | isAtom x  = SmChar (toChar x):s
  | otherwise = smToString $ toListFunction (head . smToChar) (x:s)
smToChar s               = s

-- SmOperator '▒'
smToFloat (x:s)
  | isAtom x  = SmFloat (toFloat x):s
  | otherwise = toListFunction (head . smToFloat) (x:s)
smToFloat s   = s

-- SmOperator '░'
smToInt (x:s)
  | isAtom x  = SmInt (toInt x):s
  | otherwise = toListFunction (head . smToInt) (x:s)
smToInt s     = s

-- SmOperator '▄'
smToList (x:s) = SmList (toList x):s
smToList s     = [SmList []]

-- SmOperator '█'
smToString (x:s) = SmString (toString x):s
smToString s     = [SmString ""]

-- SmOperator 't'
smTwice (q:x1:x2:s) = evalIfList1 q (x1:s):evalIfList1 q (x2:s):s
smTwice s           = smI s

-- SmOperator '\\'
smUncons (SmList (x:xs):s)   = SmList xs:x:s
smUncons (SmString (x:xs):s) = SmString xs:SmChar x:s
smUncons (x:s)
  | isAtom x                 = SmList (map SmInt [1..toInt x]):s
  | otherwise                = x:s
smUncons s                   = s

-- SmOperator '{'
smUnstack (x:s) = toList x
smUnstack s     = s

-- SmOperator 'x'
smX (q:s) = evalIfList q (q:s)
smX s     = s

-- SmOperator 'z'
smZipWith (q:x1:x2:s)
  | isAtom x1 = smZipWith (q:smRange0 (x1:x2:s))
  | isAtom x2 = smZipWith (q:x1:smRange0 (x2:s))
  | otherwise = toListFunction2 (evalIfList1 q) (x1:x2:s)
smZipWith s   = s
