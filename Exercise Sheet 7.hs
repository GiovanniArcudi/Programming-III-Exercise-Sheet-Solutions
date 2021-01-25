import Control.Monad
import Data.List

-- Exercise 1
sequenceIO_ :: [ IO a ] -> IO ()
sequenceIO_ [] = return ()
sequenceIO_ (act:acts) = do act
                            sequenceIO_ acts

putStrLn :: String -> IO ()
putStrLn s =  sequenceIO_ $ map putChar s  ++ [ putChar '\n' ]


-- Exercise 2
adderAux :: Int -> Int -> IO ()
adderAux total 0 = putStr ("The total is " ++ show total ++ "\n")
adderAux total n = do x <- getLine
                      adderAux (total + read x) (n-1)
adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getLine
           adderAux 0 (read n)


-- Exercise 3
sequenceIO :: [ IO a ] -> IO [ a ]
sequenceIO [] = return []
sequenceIO (act:acts) = do x <- act
                           xs <- sequenceIO acts
                           return (x:xs)
adder2 :: IO ()
adder2 = do putStr "How many numbers? "
            n <- getLine
            ns <- sequenceIO [ getLine | _ <- [1..read n] ]
            let total = sum $ map read ns 
            putStr ("The total is " ++ show total ++ "\n")


-- Exercise 4
queens :: Int -> [[Int]]
queens n = map fst $ foldM oneMoreQueen ([],[1..n]) [1..n]  where 
  oneMoreQueen (y,d) _ = [(x:y, delete x d) | x <- d, safe x]  where
    safe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]
 
printSolution :: [ Int ] -> IO ()
printSolution y = do
     let n = length y
     let bars = replicate (4*n+1) '-'
     mapM_ (\x -> putStrLn (bars ++ ['\n'] ++ ('|': concatMap (\c -> [' ', c ,' ', '|']) [if z == x then '\x265B' else '.' | z <- [1..n]]))) y
     putStrLn bars
     putStrLn ""
 
main = do  putStrLn "Enter the number of queens to place : "
           num <- getLine
           let parsenum = read num
           mapM_ printSolution $ queens parsenum


-- Exercise 5
import System.IO
import System.Random

main = hangman (2*)

hangman :: (Int -> Int) -> IO ()
hangman f = do word <- chooseWord
               let ds = replicate (length word) '-'
               putStrLn ds  
               putStrLn "Try to guess it:"
               play word ds (f $ length ds)

secretGetLine :: IO String
secretGetLine = do hSetEcho stdin False
                   xs <- getLine
                   hSetEcho stdin True
                   return xs

putUpdate :: String -> IO String
putUpdate s = do putStr "Your answer so far is : "
                 putStrLn s
                 return s 

play :: String -> String -> Int -> IO ()
play word answerSoFar n | n <= 0 = putStrLn "You have used up all of your guesses, game over." 
                        | answerSoFar == word = putStrLn "Correct!!"
                        | otherwise = 
                          do putStrLn ("You have " ++ (show n) ++ " guesses remaining. Enter a character :  ")
                             guess <- getChar
                             _ <- getChar
                             putStrLn ""
                             updatedAnswer <- putUpdate (updateMatch word answerSoFar guess)
                             play word updatedAnswer (n-1)

updateMatch :: String -> String -> Char -> String
updateMatch [] [] c = []
updateMatch (x:xs) (y:ys) c | x==y = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | x==c = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | otherwise = '-' : updateMatch xs ys c

 
chooseWord :: IO String
chooseWord = do handle <- openFile "Words.txt" ReadMode 
                numberOfWords <- hGetLine handle
                r <- randomRIO (0::Int,read (numberOfWords)-1)
                skipLines r handle
                word <- hGetLine handle
                putStrLn ("Secret Word Chosen.")
                return word

skipLines :: Int -> Handle -> IO ()
skipLines 0 h = return ()
skipLines n h = do hGetLine h 
                   skipLines (n-1) h


-- Exercise 6
module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


import Parsing

data BExp = Tru | Fls | Var String | And BExp BExp | Or BExp BExp
   deriving (Eq,Show)

type Substitution = [ (String, Bool) ]

check :: Maybe a -> a
check Nothing = error "No binding found for variable "
check (Just b) = b

-- evaluation function to provide semantics to formulas
eval :: Substitution -> BExp -> Bool
eval _ Tru = True
eval _ Fls = False
eval s (Var p) = check $ lookup p s
eval s (And e1 e2) = eval s e1 && eval s e2
eval s (Or e1 e2)  = eval s e1 || eval s e2

varExpr :: Parser BExp
varExpr = do s <- ident
             return (Var s)

truExpr :: Parser BExp
truExpr = do symbol "T"
             return Tru
             
flsExpr :: Parser BExp
flsExpr = do symbol "F"
             return Fls

orExpr :: Parser BExp
orExpr = do e1 <- lowerExpr
            symbol "V"
            e2 <- expr
            return (Or e1 e2)

andExpr :: Parser BExp
andExpr = do e1 <- evenLowerExpr
             symbol "&"
             e2 <- lowerExpr
             return (And e1 e2)

parenExpr :: Parser BExp
parenExpr = do symbol "("
               e <- expr
               symbol ")"
               return e
               
expr :: Parser BExp
expr = orExpr <|> lowerExpr
lowerExpr = andExpr <|> evenLowerExpr
evenLowerExpr = varExpr <|> truExpr <|> flsExpr <|> parenExpr

parseBExp :: String -> BExp
parseBExp = fst.head.parse expr
