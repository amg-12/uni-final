module Visualisation where

import qualified Data.Map as Map
import Control.Concurrent

import Syntax
import Parsing
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
logP ps = putStr $ unlines [concatMap logNode (Map.elems $ nodes p) ++ show (outs p) | p <- ps]

data VisState = X | Y | C

xMAX = 30
yMAX = 18

-- converts program output into a pixel matrix
vis :: Program -> [Int]
vis p = v X 0 0 (outs p) (replicate (xMAX*yMAX) 0)
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
render xs = concatMap ((++ "█\ESC[0m") . f) (take xMAX xs) ++ "\n" ++ render (drop xMAX xs)
  where f x = case x of
                1 -> "\x1b[38;5;239m" -- dark grey
                2 -> "\x1b[38;5;247m" -- bright grey
                3 -> "\x1b[38;5;255m" -- white
                4 -> "\x1b[31m"       -- red
                _ -> "\x1b[38;5;16m"  -- black

image :: [Program] -> IO ()
image = putStr . render . vis . last

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
  "║                   ║    \x1b[31m♥\ESC[0m ║",
  "╚═══════════════════╩══════╝"]

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

printAt :: Int -> Int -> String -> IO ()
printAt x y cs = f x (y-1) ('\n':cs)
  where f x y ('\n':cs) = do {goto x (y+1); f x (y+1) cs}
        f x y (c:cs)    = do {putStr [c]; f x y cs}
        f x y []        = return ()

renderInit :: Program -> IO [()]
renderInit p = sequence $ cls:[f i k v | (i,(k,v)) <- zip [0..] (Map.assocs $ nodes p)]
  where f i k v = do printAt (13+i*31) 10 ('@':k ++ '\n':frame)
                     printAt (15+i*31) 12 (unlines $ map show $ ops v)

renderUpdate :: Program -> IO [()]
renderUpdate p = sequence [f i k v | (i,(k,v)) <- zip [0..] (Map.assocs $ nodes p)]
  where f i k v = do printAt (35+i*31) 13 (pL 4 $ show $ get ACC v)
                     printAt (34+i*31) 16 (pL 6 $ "(" ++ show (get BAK v) ++ ")")
                     printAt (14+i*31) 12 (take 30 $ cycle " \n")
                     printAt (14+i*31) (12 + get IP v) ">"
                     printAt 13 8  (pR 100 $ show $ ins p)
                     printAt 13 29 (show $ outs p)
                     printAt 13 31 (render $ vis p)
                     goto 0 0

animate :: [Program] -> IO ()
animate ps = do {renderInit $ head ps; f ps}
  where f []     = return ()
        f (p:ps) = do renderUpdate p
                      -- threadDelay 10000
                      f ps

a :: String -> [Int] -> IO ()
a s is = do
  x <- readFile $ "progs/" ++ s
  let y = parseProgram x
  run y is def animate
