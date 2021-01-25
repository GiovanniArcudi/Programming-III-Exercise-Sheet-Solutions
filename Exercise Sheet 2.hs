--Exercise 1
result = sum [x * x + y * y * y | x <- [0 .. 100], even x, y <- [0 .. 100], odd y]


--Exercise 2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [ (x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [ (x,y) | x <- [0..n], y <- [0..n], x /= y]


--Exercise 3
replicate :: Int -> a -> [a]
replicate n x = [ x | _ <- [1..n]]


--Exercise 4
pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) |  x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z ]


--Exercise 5
perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) == x]

factors :: Int -> [Int]
factors x = [ y | y <- [1..x], x `rem` y == 0, y /= x]


--Exercise 6
positionsLecture :: Eq a => a -> [a] -> [Int]
positionsLecture x xs = [ index | (x',index) <- zip xs [0..], x==x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']


--Exercise 7
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x * y | (x,y) <- zip xs ys ]


--Exercise 8
euclid :: Int -> Int -> Int
euclid x y | (x <= 0) || (y <= 0) = error "Integers must be positive."
           | x == y               = x
           | x > y                = euclid (x-y) y
           | otherwise            = euclid x (y-x)


--Exercise 9
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstFew) (mergeSort lastFew)
    where (firstFew, lastFew) = halve xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] bs = bs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt half xs
    where half = length xs `div` 2


--Exercise A3 (100% Accuracy)
data Monotonicity = Asc | EqAsc | Desc | EqDesc | None

amSplit' :: Ord a => Monotonicity -> [a] -> [a] -> ([a], [a])
amSplit' _ ps []
    = (ps, [])     
amSplit' None [] (x:xs) 
    = amSplit' None [x] xs
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

amSplit :: Ord a => [a] -> [[a]]
amSplit xs = map reverse (amSplit'' xs)