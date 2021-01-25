{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (evalInst,findMaxReducers,isPossiblePower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


-- Exercise A7

data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

evalInst :: Stack -> SMProg -> Stack
evalInst [] _ = error "No Stack to execute!"
evalInst stack [] = stack

--Addition
evalInst [_] (Add:_) = error "Stack contains only one element!"
evalInst (Just x:Just y:stack) (Add:prog) = evalInst (Just (x+y):stack) prog
evalInst (Nothing:_:stack) (Add:prog) = evalInst (Nothing:stack) prog
evalInst (_:Nothing:stack) (Add:prog) = evalInst (Nothing:stack) prog

--Subtraction
evalInst [_] (Sub:_) = error "Stack contains only one element!"
evalInst (Just x:Just y:stack) (Sub:prog) = evalInst (Just (x-y):stack) prog
evalInst (Nothing:_:stack) (Sub:prog) = evalInst (Nothing:stack) prog
evalInst (_:Nothing:stack) (Sub:prog) = evalInst (Nothing:stack) prog

--Multiplication
evalInst [_] (Mul:_) = error "Stack contains only one element!"
evalInst (Just x:Just y:stack) (Mul:prog) = evalInst (Just (x*y):stack) prog
evalInst (Nothing:_:stack) (Mul:prog) = evalInst (Nothing:stack) prog
evalInst (_:Nothing:stack) (Mul:prog) = evalInst (Nothing:stack) prog

--Division
evalInst [_] (Div:_) = error "Stack contains only one element!"
evalInst (_:(Just 0):stack) (Div:prog) = evalInst (Nothing:stack) prog
evalInst (Just x:Just y:stack) (Div:prog) = evalInst (Just (x `div` y):stack) prog
evalInst (Nothing:_:stack) (Div:prog) = evalInst (Nothing:stack) prog
evalInst (_:Nothing:stack) (Div:prog) = evalInst (Nothing:stack) prog

--Duplication
evalInst (x:stack) (Dup:prog) = evalInst (x:x:stack) prog

--Popping
evalInst (_:stack) (Pop:prog) = evalInst stack prog




-- Exercise A8
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers s = r
where
    findMaxReducersAuxiliary :: Int -> Int -> (Maybe Stack, SMProg) -> [(Maybe Stack, SMProg)]
    findMaxReducersAuxiliary _ _ (Nothing, px) = [(Nothing, px)]
    findMaxReducersAuxiliary c n (sx, px) | c == n = [(sx,px)]
                                          | otherwise = concatMap (findMaxReducersAuxiliary (c + 1) n) ([(evalInstJust sx [p], p : px) | p <- [Add, Sub, Mul, Div, Pop]])

    takeHighestAuxiliary :: [(Maybe Stack, SMProg)] -> [(Maybe Stack, SMProg)] -> [(Maybe Stack, SMProg)]
    takeHighestAuxiliary sx1 [] = sx1
    takeHighestAuxiliary ((a,b) : sx1) ((c,d): sx2) | a > c = takeHighestAuxiliary ((a,b) : sx1) sx2
                                                    | a < c = takeHighestAuxiliary [(c,d)] sx2
                                                    | otherwise = takeHighestAuxiliary ((c,d):(a,b) : sx1) sx2

    highestPairs = takeHighestAuxiliary [(Nothing, [])] (findMaxReducersAuxiliary 0 (length s -1) (Just s, []))
    (_,t) = unzip highestPairs
    r = map reverse t

--"Maybe" version of the above evaluation function
evalInstJust :: Maybe Stack -> SMProg -> Maybe Stack
evalInstJust (Just []) _ = Nothing
evalInstJust stack [] = stack

--Addition
evalInstJust (Just [_]) (Add:_) = Nothing
evalInstJust (Just (Just x:Just y:stack)) (Add:prog) = evalInstJust (Just (Just (x+y):stack)) prog
evalInstJust (Just (Nothing:_:stack)) (Add:prog) = evalInstJust (Just (Nothing:stack)) prog
evalInstJust (Just (_:Nothing:stack)) (Add:prog) = evalInstJust (Just (Nothing:stack)) prog

--Subtraction
evalInstJust (Just [_]) (Sub:_) = Nothing
evalInstJust (Just (Just x:Just y:stack)) (Sub:prog) = evalInstJust (Just (Just (x-y):stack)) prog
evalInstJust (Just (Nothing:_:stack)) (Sub:prog) = evalInstJust (Just (Nothing:stack)) prog
evalInstJust (Just (_:Nothing:stack)) (Sub:prog) = evalInstJust (Just (Nothing:stack)) prog

--Multiplication
evalInstJust (Just [_]) (Mul:_) = Nothing
evalInstJust (Just (Just x:Just y:stack)) (Mul:prog) = evalInstJust (Just (Just (x*y):stack)) prog
evalInstJust (Just (Nothing:_:stack)) (Mul:prog) = evalInstJust (Just (Nothing:stack)) prog
evalInstJust (Just (_:Nothing:stack)) (Mul:prog) = evalInstJust (Just (Nothing:stack)) prog

--Division
evalInstJust (Just [_]) (Div:_) = Nothing
evalInstJust (Just (_:(Just 0):stack)) (Div:prog) = evalInstJust (Just (Nothing:stack)) prog
evalInstJust (Just (Just x:Just y:stack)) (Div:prog) = evalInstJust (Just (Just (x `div` y):stack)) prog
evalInstJust (Just (Nothing:_:stack)) (Div:prog) = evalInstJust (Just (Nothing:stack)) prog
evalInstJust (Just (_:Nothing:stack)) (Div:prog) = evalInstJust (Just (Nothing:stack)) prog

--Duplication
evalInstJust (Just (x:stack)) (Dup:prog) = evalInstJust (Just (x:x:stack)) prog

--Popping
evalInstJust (Just (_:stack)) (Pop:prog) = evalInstJust (Just stack) prog




-- Exercise A9
isPossiblePower :: Int -> Int -> Bool
isPossiblePower k l | k == 0            = error "k cannot be zero!"
                    | k < 0 || l < 0    = False
                    | otherwise         = isPossiblePower' k l (Just [Just 1])

isPossiblePower' :: Int -> Int -> Maybe Stack -> Bool
isPossiblePower' _ _ Nothing = False

isPossiblePower' k l stack1@(Just [Just x]) | k < 0 || l < 0            = False
                                            | k > 0 && l == 0 && x == k = True
                                            | k > 0 && l == 0 && x /= k = False
                                            | k > 0 && l > 0 && x == k  = False
                                            | k > 0 && l > 0 && x > k   = False
                                            | k > 0 && l > 0 && x < k   = isPossiblePower' k (l-1) (evalInstJust stack1 [Dup])

isPossiblePower' k l stack2@(Just ((Just x):_)) | k < 0 || l < 0            = False
                                                | k > 0 && l == 0 && x == k = False
                                                | k > 0 && l == 0 && x > k  = False
                                                | k > 0 && l == 0 && x < k  = isPossiblePower' k l (evalInstJust stack2 [Add])
                                                | k > 0 && l > 0 && x == k  = False
                                                | k > 0 && l > 0 && x > k   = False
                                                | k > 0 && l > 0 && x < k   = isPossiblePower' k (l-1) (evalInstJust stack2 [Dup]) || isPossiblePower' k l (evalInstJust stack2 [Add])
