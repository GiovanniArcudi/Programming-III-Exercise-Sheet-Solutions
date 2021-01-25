import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List


--Exercise 1

--Decide if all elements of a list satisfy a predicate
all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

allUsingFilter :: (a -> Bool) -> [a] -> Bool
allUsingFilter f [] = error "Empty List!"
allUsingFilter f xs = length xs == length $ filter f xs

--Decide if any element of a list satisfies a predicate
any  :: (a -> Bool) -> [a] -> Bool
any f = or . map f

anyUsingFilter :: (a -> Bool) -> [a] -> Bool
anyUsingFilter f [] = error "Empty List!"
anyUsingFilter f xs = not $ null $ filter f xs

--Select the initial elements from a list while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []          =  []
takeWhile' p (x:xs) | p x       =  x : takeWhile' p xs
                    | otherwise =  []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr consp []
  where consp x xs = if p x then x : xs else []

takeWhile''' :: (a -> Bool) -> [a] -> [a]
takeWhile''' p xs = foldr (\x xs -> if p x then x : xs else []) [] xs

--Remove the initial elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []          =  []
dropWhile' p l@(x:xs)
            | p x       =  dropWhile' p xs
            | otherwise =  l

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' p = id
  where dropFuns x f = if p x then f . tail else id

dropWhile''' :: (a -> Bool) -> [a] -> [a]
dropWhile f ls = foldr (\a r b -> if b && f a then r True else a:r False) (const []) ls True


--Exercise 2
dec2Int :: [Int] -> Int
dec2Int = foldl (\acc n -> acc * 10 + n) 0


--Exercise 3
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \a b -> f (a, b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(a, b) -> f a b


--Exercise 4
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin x | x < 0     = error "Only non-negative numbers accepted."
          | x == 0    = [0]
          | otherwise = reverse $ unfold (== 0) (`rem` 2) (`div` 2) x

chop :: Int -> String -> [String]
chop n xs = unfold (null) (take n) (drop n) xs

map :: (a -> b) -> [a] -> [b]
map p xs = unfold (null) (p.head) (tail) xs

iterate :: (a -> a) -> a -> [a]
iterate f x = unfold (const False) (id) (f) x


--Exercise 5
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = [] 
altMap f _ [x] = [f x]
altMap f g l@(x1:x2:xs) = f x1 : g x2 : altMap f g (tail $ tail l)


--Exercise 6
toDigitsList :: String -> [Int]
toDigitsList = map digitToInt

luhn :: String -> Bool
luhn cardNumber = (s1 + s2) `rem` 10 == 0 
  where (odds, evens) = oddsEvens (toDigitsList $ reverse cardNumber)
        s1 = sum odds
        s2 = sum $ sum . toDigitsList . show . (2 *) <$> evens
 
oddsEvens :: [a] -> ([a], [a])
oddsEvens [] = ([], [])
oddsEvens (x:xs) = (x : odds, evens)
  where (evens, odds) = oddsEvens xs


--Exercise 7
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

toTree :: Ord => [a] -> Tree a
toTree [] = Empty
toTree list = Node (toTree $ take half list)
                   (list !! half)
                   (toTree $ drop (half + 1) list)
    where half = length list `quot` 2


--Exercise 8
data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)

even' :: Nat -> Bool
even' Zero = True
even' (Succ x) = odd' x

odd' :: Nat -> Bool
odd' Zero = False
odd' (Succ x) = even' x

add' :: Nat -> Nat -> Nat
add' Zero Zero = Zero
add' (Succ x) y = Succ (add' x y)
add' x (Succ y) = Succ (add' x y)

mult' :: Nat -> Nat -> Nat
mult' Zero Zero = Zero
mult' (Succ x) y = add' y (mult' x y)
mult' x (Succ y) = add' x (mult' x y)


--Exercise 9
data RInt = Zero | Succ RInt | Pred RInt deriving Show

--Normalises an RInt (so it comprises solely successor or solely predecessor operations):
normalise :: RInt -> RInt
normalise Zero = Zero
normalise (Succ x) | isAllSucc x = Succ x
                   | otherwise   = normalise (deletePred x)
normalise (Pred x) | isAllPred x = Pred x
                   | otherwise   = normalise (deleteSucc x)

--Checks if an RInt is composed entirely of successor operations:				   
isAllSucc :: RInt -> Bool
isAllSucc Zero = True
isAllSucc (Succ x) = isAllSucc x
isAllSucc (Pred x) = False

--Checks if an RInt is composed entirely of predecessor operations:
isAllPred :: RInt -> Bool
isAllPred Zero = True
isAllPred(Pred x) = isAllPred x
isAllPred (Succ x) = False

--Deletes a successor operation in an RInt:
deleteSucc :: RInt -> RInt
deleteSucc (Succ x) = x
deleteSucc (Pred x) = Pred (deleteSucc x)

--Deletes a predecessor operation in an RInt:
deletePred :: RInt -> RInt
deletePred (Pred x) = x
deletePred (Succ x) = Succ (deletePred x)

--Checks if an RInt is odd:
odd' :: RInt -> Bool
odd' Zero = False
odd' (Succ x) = even' x
odd' (Pred x) = even' x

--Checks if an RInt is even:
even' :: RInt -> Bool
even' Zero = True
even' (Succ x) = odd' x
even' (Pred x) = odd' x

--Adds two RInts:
add' :: RInt -> RInt -> RInt
add' x Zero = x
add' x (Succ y) = Succ (add' x y)
add' (Succ x) y = Succ (add' x y)
add' x (Pred y) = Pred (add' x y)

--Subtracts one RInt from another:
subtract :: RInt -> RInt -> RInt
subtract x Zero = x
subtract x (Succ y) = Pred (subtract x y)
subtract (Succ x) y = Pred (subtract x y)
subtract x (Pred y) = Succ (subtract x y)

--Multiplies two RInts together (normalising them and calling another function):
mult :: RInt -> RInt -> RInt
mult x y = mult' (normalise x) (normalise y)

--Multiplies two normalised RInts together:
mult' :: RInt -> RInt -> RInt
mult' x Zero = Zero
mult' x (Succ y) = add' (mult' x y) x 
mult' (Succ x) y = add' (mult' x y) y 
mult' x (Pred y) = subtract (mult' x y) x


--Exercise A4 (100% Accuracy)
type Point a = (a,a)
type Metric a = Point a -> Point a -> Double

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | null xs   = []
                    | k < 0     = error "Only non-negative values of k are accepted."
                    | otherwise = take k (sortOn' (d p) xs)

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))


--Exercise A5 (100% Accuracy)
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding _ [] = Just []
findBonding p (x:xs) | null b    = Nothing
                     | otherwise = head b
                     where a = findPairs p x xs
                           b = [case bonding of {Just b -> Just (pair : (snd pair, fst pair) : b); Nothing -> Nothing} | pair <- a, let bonding = findBonding p [x | x <- xs, x /= snd pair], Data.Maybe.isJust bonding] 

findPairs :: Eq a => (a -> a -> Bool) -> a -> [a] -> [(a,a)]
findPairs p x xs = [(x,y) | y <- xs, p x y, x /= y]