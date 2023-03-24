module Interpreting where

import qualified Data.Map as Map

import Syntax

-- clamp a value to a range
clamp :: Int -> Int -> Int -> Int
clamp l h x = max l $ min h x

-- change value of internal register
update :: Register -> Int -> Node -> Node
update NIL _ n = n
update r   x (N os s m) = N os s' m
  where s' r' | r' == r   = clamp (-999) 999 x
              | otherwise = s r'

-- check value of internal register
get :: Register -> Node -> Int
get r n = state n r

-- number of lines in a node
len :: Node -> Int
len = length . ops

-- current line of a node
current :: Node -> Operation
current (N os s Default)     = os !! s IP
current (N os _ (Write i d)) = O (Mov d) (Cons i)

-- set mode to writing or not writing
setMode :: Mode -> Node -> Node
setMode m (N os s _) = N os s m

-- safely set IP to next line
inc :: Node -> Node
inc n = setMode Default $ update IP ((get IP n + 1) `mod` len n) n

-- safely set IP to any line
jump :: Int -> Node -> Node
jump i n = update IP (clamp 0 (len n-1) i) n

-- check if a node or input is being read by any node
checkAnyRead :: Operand -> Program -> Bool
checkAnyRead o p = any ((== o) . operand . current) (Map.elems $ nodes p)

-- check if a certain node or output is reading another certain node
checkRead :: Operand -> Operand -> Program -> Bool
checkRead (Port s) o (Pr ns _ _) = operand (current (ns Map.! s)) == o
checkRead OUT _ _ = True

-- gather all the values being written to a node or output
readAny :: Operand -> Program -> [Int]
readAny o p = [f $ operand $ current a | a <- Map.elems $ nodes p, operator (current a) == Mov o]
  where f (Cons x) = x

-- evaluate an operand into a constant
eval :: Operand -> Operand -> Node -> Program -> Maybe Int
eval (Cons x) _ _ _ = Just x
eval (Regi r) o n _ = Just $ get r n
eval  IN      _ _ (Pr _ (i:is) _) = Just i
eval  IN      _ _ (Pr _ []     _) = Nothing
eval (Port x) o _ (Pr ns _ _) = 
  case current (ns Map.! x) of
    O (Mov o') (Cons y) -> if o == o' then Just y else Nothing
    _                   -> Nothing

-- internally execute a line of code
e :: Operator -> Int -> Node -> Node
e (MOV (Regi r)) x n = update r x $ inc n
e (MOV d) x n = setMode (Write x d) n
e (Mov _) _ n = n
e  SWP    _ n = update BAK (get ACC n) $ update ACC (get BAK n) $ inc n
e  ADD    x n = update ACC (get ACC n + x) $ inc n
e  SUB    x n = update ACC (get ACC n - x) $ inc n
e  NEG    _ n = update ACC (negate $ get ACC n) $ inc n
e (JRO c) x n = if c $ get ACC n then jump (get IP n + x) n else inc n
e  HCF    _ _ = error "halted and caught fire"

-- process a program for 1 cycle
step :: Program -> Program
step p = Pr (Map.fromList [f n | n <- Map.toList $ nodes p]) 
           (if not (null $ ins p) && checkAnyRead IN p then tail $ ins p else ins p)
           (outs p ++ readAny OUT p)
  where f (k, v) = case ((operator . current) v, eval ((operand . current) v) (Port k) v p) of
          (Mov r, _)  -> if checkRead r (Port k) p then (k, inc v) else (k, v)
          (o, Just x) -> (k, e o x v)
          _           -> (k, v)
          
type HaltCondition = [Program] -> [Program]

input :: [Int] -> Program -> Program
input is (Pr ns _ os) = Pr ns is os

-- runs a program infinitely or until a condition is met. takes a continuation
run :: Program -> [Int] -> HaltCondition -> ([Program] -> a) -> a
run p is c f = f $ c $ iterate step $ input is p

-- program runs until all nodes are blocked
stuck :: HaltCondition
stuck = takeWhile (\p -> p /= step p)

timeout :: Int -> HaltCondition
timeout = take

def :: HaltCondition
def = stuck . timeout 10000

results :: [Program] -> [Int]
results = outs . last