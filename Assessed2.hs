{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Ord (comparing)
import qualified Data.Maybe



-- Exercise A4

type Point a = (a,a)
type Metric a = Point a -> Point a -> Double

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | null xs   = []
                    | k < 0     = error "Only non-negative values of k are accepted."
                    | otherwise = take k (sortOn' (d p) xs)

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f =
  map snd .
  sortBy (comparing fst) .
  map (\x -> let y = f x in y `seq` (y, x))




-- Exercise A5

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding _ [] = Just []
findBonding p (x:xs) | null b = Nothing
                     | otherwise = head b
                     where a = findPairs p x xs
                           b = [case bonding of {Just b -> Just (pair : (snd pair, fst pair) : b); Nothing -> Nothing} | pair <- a, let bonding = findBonding p [x | x <- xs, x /= snd pair], Data.Maybe.isJust bonding] 

findPairs :: Eq a => (a -> a -> Bool) -> a -> [a] -> [(a,a)]
findPairs p x xs = [(x,y) | y <- xs, p x y, x /= y]




-- Exercise A6

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v (Leaf, []) = putIn v (Leaf, [])
insertFromCurrentNode v zi = compare' v zi

putIn :: a -> Zipper a -> Zipper a
putIn v = attach (Node Leaf v 1 Leaf)

attach :: VTree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs) 

getVal :: (VTree a, b) -> a
getVal (Node _ x _ _, _) = x

compare' :: Ord a => a -> Zipper a -> (VTree a, Trail a)
compare' v zi | v == getVal zi                              = zi
              | v < getVal zi && canUp zi && v > getVal up  = putT v up
              | v > getVal zi && canUp zi && v < getVal up  = putT v up
              | canUp zi                                    = compare' v up
              | otherwise                                   = putT v zi
              where up = goUp zi

putT :: Ord a => a -> Zipper a -> Zipper a
putT v zi | (getNode left /= Leaf) && v < getVal zi  = putT v left
          | (getNode right /= Leaf) && v > getVal zi = putT v right
          | (getNode left == Leaf) && v < getVal zi  = putIn v left
          | otherwise                                = putIn v right
          where left = goLeft zi
                right = goRight zi


getNode :: (a, b) -> a
getNode (t, _) = t

canUp :: Zipper a -> Bool
canUp ( _ , [] ) = False
canUp ( _ , _ ) = True

modify :: Zipper a -> Zipper a
modify (Node l x i r, ts) = (Node l x (i+1) r, ts)
modify (Leaf, ts) = (Leaf, ts) 

goLeft,goRight,goUp :: Zipper a -> Zipper a
goLeft (Node l x i r , ts) = modify (l , L x i r:ts)
goRight (Node l x i r , ts) = modify (r , R x i l:ts)
goUp (t , L x i r : ts) = (Node t x (i+1) r , ts)
goUp (t , R x i l : ts) = (Node l x (i+1) t , ts)