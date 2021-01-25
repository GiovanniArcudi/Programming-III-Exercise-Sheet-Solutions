import Data.Char ( ord, chr )


--Exercise 1

--(a)
mylast1 :: [a] -> a
mylast1 [] = error "No empty list allowed."
mylast1 [x] = x
mylast1 (_:xs) = mylast1 xs

--(b)
mylast2 :: [a] -> a
mylast2 [] = error "No empty list allowed."
mylast2 xs = head $ reverse xs


--Exercise 2

--(a) -- If list is smaller than 4, we get empty list error.
fourth1 :: [a] -> a
fourth1 xs = head (tail (tail (tail xs)))

--(b) --If list is smaller than 4, we get index too large error.
fourth2 :: [a] -> a
fourth2 xs = xs !! 3  

--(c) --If list is smaller than 4, we get non-exhaustive patterns in fuction fourth3.
fourth3 :: [a] -> a
fourth3 (_:_:_:x:_) = x


--Ecercise 3

--(a)
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

--(b)
safetail2 :: [a] -> [a]
safetail2 xs | null xs  = xs
             | otherwise = tail xs
                 
--(c) --This is the most concise approach.
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs 


--Exercise 4
halve :: [a] -> ([a],[a])
halve xs = splitAt half xs
           where half = length xs `div` 2


--Exercise 5
enc :: Int -> String -> String
enc _ [] = []
enc n (x:xs) = chr (n + ord x) : enc n xs

encrypt :: Int -> String -> (String , String -> String)
encrypt n xs = (enc n xs, dec)
    where dec (x:xs) = chr (- n + ord x) : dec xs

--Use this function if you want to check that what we are doing in encrypt works properly.
--dec :: Int -> String -> String
--dec _ [] = []
--dec n (x:xs) = [chr (-n + ord x)] ++ decr n xs


--Exercise 6
luhnDouble :: Int -> Int
luhnDouble n | 2*n > 9   = 2*n -9
             | otherwise = 2*n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 | luhnSum `mod` 10 == 0 = True
                 | otherwise             = False
                 where luhnSum = luhnDouble x1 + x2 + luhnDouble x3 + x4


--Exercise A1 (100% Accuracy)
histogram :: Int -> [Int] -> [Int]
histogram n xs = if n > 0 then [sum [count xi xs | xi <- x] | n > 0, x <- intervals n xs]
                 else error "input not valid" 
          
intervals n xs = [getLength n max| max <- [0..(maximum xs `div` n)]]

getLength n max = [n*max..(n*(max+1) - 1)]

count x xs = length[x' | x' <- xs, x == x']


--Exercise A2 (100% Accuracy)
approxPi :: Int -> Double
approxPi n | n < 0     = error "Input should not be a negative number."
           | n == 0    = error "Input should not be zero."
           | otherwise = 2 * sum [fromInteger (factorial k) / fromInteger (doublefactorial (2*k + 1)) | k <- [0..n-1] ]

factorial :: Int -> Integer
factorial n = product [1..(toInteger n)]

doublefactorial :: Int -> Integer
doublefactorial 0 = 1
doublefactorial 1 = 1
doublefactorial n = toInteger n * doublefactorial (n-2)