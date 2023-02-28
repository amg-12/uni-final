module Parsing where

import Text.Read
import Data.Maybe
import Control.Monad
import Data.Either
import qualified Data.Map as Map

import Hutton
import Syntax

goodchar :: Parser Char
goodchar = sat (`notElem` " :#\r\n")

word :: Parser String
word = token $ many goodchar

name :: Parser String
name = token $ do char '@'
                  word

register :: Parser Register
register = do symbol "ACC"
              return  ACC
       <|> do symbol "NIL"
              return  NIL

pOperand :: Parser Operand
pOperand =  Regi <$> register
        <|> Cons <$> integer
        <|> Port <$> name
        <|> do symbol "IN"
               return  IN
        <|> do symbol "OUT"
               return  OUT    

operation :: Parser Operation
operation =  do symbol "MOV"
                o <- pOperand
                r <- pOperand
                return $ O (MOV r) o
         <|> do symbol "ADD"
                O ADD <$> pOperand
         <|> do symbol "SUB"
                O SUB <$> pOperand
         <|> do symbol "JRO"
                O (JRO (const True)) <$> pOperand
         <|> do symbol "JMP"
                w <- word
                return $ O (Jmp (const True) w) (Regi NIL)
         <|> do symbol "JEZ"
                w <- word
                return $ O (Jmp (==0)        w) (Regi NIL)
         <|> do symbol "JNZ"
                w <- word
                return $ O (Jmp (/=0)        w) (Regi NIL)
         <|> do symbol "JGZ"
                w <- word
                return $ O (Jmp (>0)         w) (Regi NIL)
         <|> do symbol "JLZ"
                w <- word
                return $ O (Jmp (<0)         w) (Regi NIL)
         <|> do symbol "SAV"
                return $ O (MOV $ Regi BAK) (Regi ACC)
         <|> do symbol "NOP" 
                return $ O ADD (Regi NIL)
         <|> do xs <- word
                guard $ isJust (readMaybe xs :: Maybe Operator)
                return $ O (read xs :: Operator) (Regi NIL)

                
label :: Parser Label
label = token $
          do w <- word
             char ':'
             return w

node :: Parser [Either Operation Label]
node = many $
        Left  <$> operation
    <|> Right <$> label

namedNode :: Parser (String, [Either Operation Label])
namedNode = do x <- name
               y <- node
               return (x, y) 

-- consume labels and replace them with relative offsets
process :: [Either Operation Label] -> (Label -> Int) -> [Operation] -> [Operation]
process [] f os = p os
  where p [] = []
        p xs = p (init xs) ++ [q (last xs)]
          where q (O (Jmp c s) _) = O (JRO c) (Cons (f s - length (init xs)))
                q x         = x
process (Left  o : es) f n = process es f (n ++ [o])
process (Right l : es) f n = process es f' n
  where f' l' | l' == l   = if all isRight es then 0 else length n
              | otherwise = f l'
           
program :: Parser [(String, [Either Operation Label])]
program = many namedNode

processProgram :: [(String, [Either Operation Label])] -> Map.Map String Node
processProgram = foldr (\ x -> Map.insert (fst x) (N (process (snd x) (const $ error "???") []) (const 0) Default)) Map.empty

parseProgram :: String -> Program
parseProgram x = Pr ((processProgram . fst . head . parse program) x) [] []