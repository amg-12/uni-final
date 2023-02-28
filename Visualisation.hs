module Visualisation where

import qualified Data.Map as Map

import Syntax
import Interpreting

-- adapted from Data.List.HT
pR :: Int -> String -> String
pR n xs = take n $ xs ++ repeat ' '

pL :: Int -> String -> String
pL n xs = replicate (n - length xs) ' ' ++ xs

logNode :: Node -> String
logNode n = concat [
  pL  2 $ show $ get IP n,  ": ",
  pR 12 $ show $ current n, "|",
  pL  4 $ show $ get ACC n, "| (",
  pL  4 $ show $ get BAK n, ")       "]

logP :: [Program] -> IO ()
logP ps = putStr $ unlines [concatMap logNode (Map.elems $ nodes p) ++ show (ins p) ++ show (outs p) | p <- ps]

data VisState = X | Y | C

xMAX = 30
yMAX = 18

-- converts program output into a pixel matrix
vis :: [Program] -> [Int]
vis ps = v X 0 0 (results ps) (replicate (xMAX*yMAX) 0)
  where
    v :: VisState -> Int -> Int -> [Int] -> [Int] -> [Int]
    v _ _ _ [] i = i
    v s x y (a:as) i = case (s, x<xMAX && y<yMAX, a<0) of
      (_, _, True)  -> v X 0 0 as i
      (X, _, _)     -> v Y a 0 as i
      (Y, _, _)     -> v C x a as i
      (C, False, _) -> v C x y as i
      (C, True, _)  -> v C (x+1) y as $ take (xMAX*y+x) i ++ [a] ++ drop (xMAX*y+x+1) i

render :: [Int] -> String
render [] = []
render xs = concatMap ((++ "█") . f) (take xMAX xs) ++ "\n" ++ render (drop xMAX xs)
  where f x = case x of
                1 -> "\x1b[38;5;239m" -- dark grey
                2 -> "\x1b[38;5;247m" -- bright grey
                3 -> "\x1b[38;5;255m" -- white
                4 -> "\x1b[31m"       -- red
                _ -> "\x1b[38;5;16m"  -- black

image :: [Program] -> IO ()
image = putStr . render . vis

frame = unlines [ -- 29x17
  "╔═══════════════════╦══════╗",
  "║                   ║ ACC  ║",
  "║                   ║      ║",
  "║                   ╠══════╣",
  "║                   ║ BAK  ║",
  "║                   ║      ║",
  "║                   ╠══════╣",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "║                   ║      ║",
  "╚═══════════════════╩══════╝"]
  
replace :: Int -> [a] -> [a] -> [a]
replace n x y = take n y ++ x ++ drop (n + length x) y

renderNode :: Node -> String
renderNode n = foldl (\x f -> f x) frame $
  [replace (29*(p+1)+1) (take 18 $ show l) | (p, l) <- zip [0..] (ops n)] ++
  [replace 80  (pL 4 $ show (get ACC n))] ++
  [replace 166 (pL 6 $ "(" ++ show (get BAK n) ++ ")")]
  
renderProgram :: Program -> IO ()
renderProgram p = putStr $ concatMap renderNode (nodes p)