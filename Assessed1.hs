{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (histogram,approxPi,amSplit) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


-- Exercise A1

histogram :: Int -> [Int] -> [Int]
histogram n xs = if n > 0 then [sum[count xi xs | xi <- x] | x <- intervals n xs, n > 0]
                 else error "input not valid" 
          
intervals n xs = [getLength n max | max <- [0..((maximum xs) `div` n)]]

getLength n max = [n*max..(n*(max+1) - 1)]

count x xs = length[x' | x' <- xs, x == x']




-- Exercise A2

approxPi :: Int -> Double
approxPi n | n < 0     = error "Input should not be a negative number."
           | n == 0    = error "Input should not be zero."
           | otherwise = 2 * sum [fromInteger (factorial k) / fromInteger (doublefactorial (2*k + 1)) | k <- [0..n-1]]

factorial :: Int -> Integer
factorial n = product [1..(toInteger n)]

doublefactorial :: Int -> Integer
doublefactorial 0 = 1
doublefactorial 1 = 1
doublefactorial n = (toInteger n) * doublefactorial (n-2)




-- Exercise A3

data Monotonicity = Asc | EqAsc | Desc | EqDesc | None

amSplit :: Ord a => [a] -> [[a]]
amSplit xs = map reverse (amSplit'' xs)

amSplit' :: Ord a => Monotonicity -> [a] -> [a] -> ([a], [a])
amSplit' _ ps [] = (ps, [])     
amSplit' None [] (x:xs) = amSplit' None [x] xs
amSplit' None (p:ps) (x:xs)
    | p < x     = amSplit' Asc   (x:p:ps) xs
    | p > x     = amSplit' Desc  (x:p:ps) xs
    | otherwise = amSplit' None  (x:p:ps) xs
amSplit' Asc  (p:ps) (x:xs)
    | p < x     = (p:ps, x:xs)
    | p == x    = amSplit' EqAsc (x:p:ps) xs
    | otherwise = amSplit' Desc (x:p:ps) xs
amSplit' Desc (p:ps) (x:xs)
    | p > x     = (p:ps, x:xs)
    | p == x    = amSplit' EqDesc (x:p:ps) xs
    | otherwise = amSplit' Asc (x:p:ps) xs
amSplit' EqAsc (p:ps) (x:xs)
    | p < x     = (p:ps, x:xs)
    | p == x    = amSplit' EqAsc (x:p:ps) xs
    | otherwise = amSplit' Desc  (x:p:ps) xs
amSplit' EqDesc (p:ps) (x:xs)
    | p > x     = (p:ps, x:xs)
    | p == x    = amSplit' EqDesc (x:p:ps) xs
    | otherwise = amSplit' Asc    (x:p:ps) xs

amSplit'' :: Ord a => [a] -> [[a]]
amSplit'' xs
    | null prefix = []
    | null rest   = [prefix]
    | otherwise   = prefix:amSplit'' rest
    where
        (prefix, rest) = amSplit' None [] xs

