-- Exercise 1
-- Define the append function as
-- 
--     [ ] ++ ys = ys
--  (x:xs) ++ ys = x : (xs ++ ys)
-- 
-- and use equational reasoning to show that
-- 
-- xs ++ [ ] = xs and
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs.
--
-- Reason by induction on List x
-- Base case, xs is []
-- [] ++ [] 
-- = []  by defintion  ->
-- 
-- Inductive case, suppose xs ++ [] = xs
-- (x:xs) ++ [] 
--   = x : (xs ++ [])    by definition ->
--   = x : xs  by the inductive hypothesis
-- 
-- Reason by induction on List x
-- Base case, xs is []
-- [] ++ (ys ++ zs) 
--   = ys ++ zs  by definition ->
--   =  ([] ++ ys) ++ zs  by definition <-
-- 
-- Inductive case, suppose xs ++ (ys ++ zs) = (xs + ys) ++zs
-- (x:xs) ++ (ys ++ zs) 
--  = x : (xs ++ (ys ++ zs))  by definition ->
--  = x : ((xs ++ ys) ++ zs)  by the inductive hypothesis
--  = (x : (xs ++ ys))  ++ zs by definition <-
--  = ((x:xs) ++ ys) ++ zs by definition <-


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 2

-- Remember the data type data Nat = Zero | Succ Nat. We can use this to act as a simple counter for functions. For example, we can write

replicate :: Nat -> a -> [a]
replicate Zero _ = []
replicate (Succ n) x = x : replicate n x

-- We wish to prove that replicate produces a list whose elements are all equal to the element given to replicate. To do this we can define the function

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

-- and prove that all (== x) (replicate n x) = True for any x and any n. Write this proof. What assumptions do you need to make about (==) in order for the proof to hold? Are these valid assumptions?

-- Prove that all (== x) (replicate n x) = True for any x,n
-- Reason by induction on Nat n.

-- Base Case, n is Zero
-- all (== x) (replicate Zero x)
--  = all (== x) []  by definition of replicate ->
--  = True  by definition of all ->
-- 
-- Inductive case, suppose: all (== x) (replicate n x) = True
-- all (== x) (replicate (Succ n) x)
--  = all (== x) (x : replicate n x)  by definition ->
--  = (== x) x && all (== x) (replicate n x) by definition ->
--  = (== x) x && True  by the inductive hypothesis
--  =  x == x  rewriting as infix
--  =  True    by assumption on ==, yes, this is reasonable due to Eq.


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 3

-- Prove that for any list we have take n xs ++ drop n xs = xs where

take Zero _  = []
take _ [] = []
take (Succ n) (x:xs) = x : take n xs

drop Zero xs = xs
drop _ [] = []
drop (Succ n) (_:xs) = drop n  xs

 
-- Prove that take n xs ++ drop n xs = xs  for any xs, n
-- Reason by induction on n
--Base case n is Zero
--take Zero xs ++ drop Zero xs 
-- = [] ++ drop Zero xs   by definition of take ->
-- = [] ++ xs             by definition of drop ->
-- = xs                   by Exercise One
--
--Inductive case, suppose take n xs ++ drop n xs = xs
--Suppose list is empty
--take (Succ n) [] ++ drop (Succ n) [] 
-- = [] ++ drop (Succ n) []   by definition of take ->
-- = [] ++ []                 by definition of drop ->
-- = []                       by Exercise One
--
--Suppose list is not empty.
--take (Succ n) (x:xs) ++ drop (Succ n) (x:xs) 
-- = x : (take n xs) ++ drop (Succ n) (x:xs)  by definition of take ->
-- = (x : (take n xs)) ++ drop n xs           by definition of drop ->
-- = x : ( take n xs ++ drop n xs )           by definition of ++  ->
-- = x : xs                                   by the inductive hypothesis


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 4

--Define the following functions over Nat

-- even :: Nat -> Bool that determines whether a Nat represents an even number.
-- double :: Nat -> Nat that doubles a given Nat

--Prove that even (double n) = True for any n.

data Nat = Zero | Succ Nat deriving Show

even :: Nat -> Bool
even Zero = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero = False
odd (Succ n) = even n

double Zero = Zero 
double (Succ n) = Succ (Succ (double n))

-- Prove that even (double n) = True  for any n
-- Use induction on n

-- Base case
--   even (double Zero)
-- = even (add Zero Zero)
-- = even (Zero)
-- = True
--
-- Ind. Case 
-- even (double (Succ n))
--   = even (Succ (Succ (double n)))
--   = odd (Succ (double n))
--   = even (double n)
--   = True
--
--
-- Ind. Case
--   even (double (Succ n))
-- = even (add (Succ n) (Succ n))
-- = even (Succ (add n (Succ n)))
-- = not ( even (add n (Succ n)) )
-- = {lemma} not (even ( Succ (add n n) ) )
-- = not (not ( even (add n n) ) )
-- = not (not ( even (double n) ) )
-- = {IH} not (not ( True )
-- = not False
-- = True
--
--
-- Lemma :  add n (Succ m) = Succ ( add n m )
-- Proof by induction on n.
--
-- Base
--    add Zero (Succ m)
-- = Succ m
-- = Succ (add Zero m)
--
-- Ind Case
--    add (Succ n) (Succ m)
--  = Succ ( add n (Succ m) )
--  = {IH} Succ ( Succ ( add n m ) )
--  = Succ ( add (Succ n) m )


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 5

-- Using the data type data Tree a = Leaf a | Node (Tree a) (Tree a) show that in any such tree the number of leaves is always one greater than the number of nodes.

leaves :: Tree a -> Int
leaves (Leaf a) = 1
leaves (Node l r) = (leaves l) + (leaves r)

nodes :: Tree a -> Int
nodes (Leaf a) = 0
nodes (Node l r) = (nodes l) + (nodes r) + 1

-- Prove that :  leaves t = nodes t + 1 for any tree t
-- Reason by induction over the Tree t.
-- 
-- Base Case:  t is a Leaf node
-- 
-- leaves (Leaf a)
-- = 1
-- = 0 + 1
-- = nodes (Leaf a) + 1
-- 
-- Inductive Case : t is of the form Node l r
--
--
-- Suppose as the Inductive hypothesis that 
-- 
-- IH1 :  leaves l = (nodes l) + 1
-- IH2 :  leaves r = (nodes r) + 1
-- 
-- Now prove the property for t:
-- leaves (Node l r)
-- = (leaves l) + (leaves r)   by defn of leaves
-- = (nodes l) + 1 + (leaves r)  by IH1
-- = (nodes l) + 1 + (nodes r) + 1 by IH2
-- = ((nodes l) + (nodes r) + 1) + 1 by arithmetic of +
-- = (nodes (Node l r)) + 1 by defn of nodes


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 6

-- Verify the functor laws for the functors Maybe and Tree a where

instance Functor Maybe where
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap g Nothing = Nothing
fmap g (Just x) = Just (g x) 

instance Functor Tree where
fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x) = Leaf (g x)
fmap g (Node l r) = Node (fmap g l) (fmap g r) 

-- Functor Law 1 for Maybe
fmap id Nothing = Nothing = id Nothing
fmap id (Just x) = Just (id x) = Just x = id (Just x)
-- Hence fmap id = id

-- Functor Law 2 for Maybe 
fmap (f . g) Nothing 
  = Nothing 
  = fmap f Nothing
  = fmap f (fmap g Nothing)
  = (fmap f . fmap g) Nothing
fmap (f . g) (Just x)
  = Just ((f . g) x)
  = Just (f (g x))
  = fmap f (Just g x)
  = fmap f (fmap g (Just x))
  = (fmap f . fmap g ) (Just x)
-- Hence  fmap (f.g) = (fmap f).(fmap g)

-- Functor Law 1 for Tree
-- Reason by induction on the Tree
-- Base Case
fmap id (Leaf x) = Leaf (id x) = (Leaf x) = id (Leaf x)
-- Inductive Case, suppose (fmap id l) = id l and fmap id r = id r
fmap id (Node l r) 
  = Node (fmap id l) (fmap id r)) 
  = Node (id l) (id r)
  = Node l r
  = Id (Node l r)

-- Functor Law 2 for Tree
-- Reason by induction on the Tree
-- Base Case
fmap (f.g) (Leaf x) 
 = Leaf (f.g x) 
 = Leaf (f (g x))
 = fmap f (Leaf (g x))
 = fmap f (fmap g (Leaf x))
 = (fmap f) . (fmap g) (Leaf x)

-- Inductive Case, suppose 
fmap (f.g) l = (fmap f) . (fmap g) l and
fmap (f.g) r = (fmap f) . (fmap g) r then
fmap (f.g) (Node l r)
 = Node (fmap (f.g) l) (fmap (f.g) r)
 = Node ((fmap f) . (fmap g) l ) ((fmap f).(fmap g) r)
 = Node ((fmap f) (fmap g l))  ( (fmap f) (fmap g r) )
 = fmap f (Node (fmap g l) (fmap g r))
 = fmap f ( fmap g ( Node l r ))
 = (fmap f).(fmap g) (Node l r) 
-- Hence fmap f.g = fmap f . fmap g


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 7

-- Identify the redexes in the following expressions and categorise them as innermost, outermost, neither or both

-- 1 + (2 * 3)
-- (1 + 2) * (2 + 3)
-- fst (1 + 2 , 2 + 3)
-- (\x -> 1 + x) ( 2 * 3 )


-- Redexes in 1 + 2 * 3
-- rewrite as (+) 1 ( (*) 2 3 )
-- (+) 1   - innermost , outermost
-- (*) 2   - innermost , outermost

-- Redexes in (1 + 2) * (2 + 3)
-- (+) 1  -  innermost
-- (+) 2  - innermost, outermost 
-- (*) (1 + 2) -  outermost

-- Redexes in fst (1 + 2 , 2 + 3)
-- (+) 1 - innermost 
-- (+) 2 - innermost
-- fst (1 + 2 , 2 + 3) - outermost

-- Redexs in (\x -> 1 + x) ( 2 * 3 )
-- (*) 2 - innermost
-- (+) 1 - innermost
-- (\x -> 1 + x) ( 2 * 3 )  - outermost 

-- Under a CBV strategy (*) 2 would be the redex selected next


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 8

-- Define mult = \x -> (\y -> x * y) and show all of the evaluation steps that Haskell would take when evaluating mult (mult 3 4) 5

-- mult ( mult 3 4 ) 5 
-- = (\x -> (\y -> x * y)) (mult 3 4) 5
-- = (\y -> (mult 3 4) * y) 5
-- = (mult 3 4) * 5
-- =  (*) (mult 3 4) 5
-- =  (*) (\x -> (\y -> x * y)) 3 4 ) 5
-- =  (*) ((\y -> 3 * y) 4) 5
-- =  (*) (3 * 4) 5
-- =  (*) ((*) 3 4) 5


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 9

-- Use a list comprehension to define the infinite Fibonacci sequence 0,1,1,2,3,5,8,13,21,34 ... You will find it useful to use zip and tail and you may want to use the type Integer rather than Int.

fibs :: [Integer]
fibs = 0 : 1 : [ n + m |  (n,m) <- zip fibs (tail fibs) ]


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercise 10

-- Recall the functions repeat :: a -> [a] and take :: Int -> [a] -> [a] for lists? 
-- They produce an infinite list of copies of the given element and a prefix of a given size of a list respectively.
-- We can compose them to define replicate n a that produces a fixed size list of copies of a.
-- Write analagous functions

repeatTree :: a -> Tree a
takeTree :: Int -> Tree a -> Tree a and
replicateTree :: Int -> a -> Tree a

-- that work on the Tree data type. Make sure you define the latter function modularly.

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq,Ord,Show,Read)

repeatTree :: a -> Tree a
repeatTree a = Node ( repeatTree a ) a ( repeatTree a )

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf  
takeTree _ Leaf = Leaf
takeTree n (Node l v r) = Node (takeTree (n-1) l ) v ( takeTree (n-1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree