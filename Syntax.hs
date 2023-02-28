{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Syntax where

import qualified Data.Map as Map

data Register = ACC
              | NIL
           -- non-addressable:
              | BAK
              | IP
              deriving (Show, Read, Eq)

data Operand = Regi Register
             | Cons Int
             | Port String
             | IN  -- also ports
             | OUT
             deriving (Read, Eq)

type Condition = Int -> Bool
type Label = String

data Operator = MOV Operand
              | SWP
              | ADD
              | SUB
              | NEG
              | JRO Condition
              | HCF
           -- runtime only:
              | Mov Operand
           -- parsing only
              | Jmp Condition Label
           -- replaced with equivalent:
           -- | NOP                         -> ADD NIL
           -- | SAV                         -> MOV ACC BAK
           -- | JMP | JEZ | JNZ | JGZ | JLZ -> JRO
              deriving (Show, Read, Eq)

data Operation = O
                 { operator :: Operator
                 , operand  :: Operand
                 } deriving (Read, Eq)

type State = Register -> Int

data Mode = Default
          | Write Int Operand
          deriving (Show, Eq)

data Node = N
            { ops   :: [Operation]
            , state :: State 
            , mode  :: Mode
            } deriving Eq

data Program = Pr
               { nodes :: Map.Map String Node
               , ins   :: [Int]
               , outs  :: [Int]
               } deriving Eq


-- instances

instance Show Operand where
  show (Regi x) = show x
  show (Cons x) = show x
  show (Port x) = "@" ++ x
  show IN       = "IN"
  show OUT      = "OUT"

instance Show Operation where
  show (O (MOV o) x) = "MOV " ++ show x ++ " " ++ show o
  show (O (Mov o) x) = "Mov " ++ show x ++ " " ++ show o
  show (O x y) = show x ++ " " ++ show y

instance Show Condition where
  show c = case map c [(-1)..1] of
             [True, True, True ] -> "__"
             [False,True, False] -> "EZ"
             [True, False,True ] -> "NZ"
             [False,False,True ] -> "GZ"
             [True, False,False] -> "LZ"

instance Read Condition where
  readsPrec _ _ = []
  
instance Eq Condition where
  x == y = map x [(-1)..1] == map y [(-1)..1]

instance Eq State where
  x == y = map x [ACC, BAK, IP] == map y [ACC, BAK, IP]

instance Show Node where
  show = show . ops
  
instance Show Program where
  show (Pr ns is os) = show ns ++ show is ++ show os