--Exercise 1
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq,Ord,Show,Read)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                             EQ -> True
                             LT -> occurs x l
                             GT -> occurs x r


--Exercise 2
foldTree :: (a -> b) -> (b -> a -> b -> b) -> Tree a -> b
foldTree leaff _ (Leaf x) = leaff x
foldTree leaff nodef (Node left x right) = nodef (foldTree leaff nodef left) x (foldTree leaff nodef right)

flatten :: Tree a -> [a]
flatten = foldTree (: []) (\ls x rs -> ls ++ (x:rs))


--Exercise 3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr
foldExpr :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr valFunc _ _ (Val x) = valFunc x
foldExpr valFunc addFunc subFunc (Add e1 e2) = addFunc (foldExpr valFunc addFunc subFunc e1) (foldExpr valFunc addFunc subFunc e2)
foldExpr valFunc addFunc subFunc (Sub e1 e2) = subFunc (foldExpr valFunc addFunc subFunc e1) (foldExpr valFunc addFunc subFunc e2)

eval :: Expr -> Int
eval = foldExpr id (+) (-) 

size :: Expr -> Int
size = foldExpr (const 1) (\x y -> x + y + 1) (\x y -> x + y + 1)

testExpr = Val 1
testExpr2 = Add (Val 1) (Val 2)
testExpr3 = Sub (Val 2) (Val 1)
testExpr4 = Add (Val 1) (Sub (Val 2) (Val 1))


--Exercise 4
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
data Form = Positive | Negative | Mixed | Either deriving Show

getForm :: Prop -> Form
getForm (Const _) = Either
getForm (Var _) = Positive 
getForm (Not p) = negateForm $ getForm p
getForm (And p q) =  andForm (getForm p) (getForm q)
getForm (Imply p q) = andForm (negateForm $ getForm p) (getForm q)

negateForm :: Form -> Form
negateForm Negative = Positive
negateForm Positive = Negative
negateForm Either = Either
negateForm Mixed = Mixed

andForm :: Form -> Form -> Form
andForm Positive Positive = Positive
andForm Negative Negative = Negative
andForm f Either = f
andForm Either f = f
andForm _ _ = Mixed


--Exercise 5

--Define the data type data Pair a b = P (a, b).
--Write code to make the type constructor (Pair a) into a Functor.
newtype Pair a b = P (a, b)

instance Functor (Pair a) where
    fmap f (P (x,y)) = P (x, f y)

--Define the data type data Fun a b = F (a -> b).
--Write code to make the type constructor (Fun a) into a Functor.
newtype Fun a b = F (a -> b)

instance Functor (Fun a) where
    fmap f (F g) = F (f . g)


--Exercise 6
data LTree a = LLeaf a | LNode (LTree a) (LTree a)
data Direction a = L (LTree a) | R (LTree a)
type Trail a = [Direction a]
type Zipper a = (LTree a, Trail a)

inc2LR :: LTree Int -> LTree Int
inc2LR (LLeaf a) = LLeaf a
inc2LR a = composition(a, [])
composition = fst.goToRoot.increment.goFullyRight.goLeft.goUp.goFullyRight.goToRoot.increment.goFullyLeft.goRight.goUp.goFullyLeft

goLeft :: Zipper a -> Zipper a
goLeft (LNode x y , ts) = (x, L y :ts)

goFullyLeft :: Zipper a -> Zipper a
goFullyLeft (LLeaf x , ts) = (LLeaf x , ts)
goFullyLeft (LNode x y , ts) = goFullyLeft (goLeft (LNode x y , ts))

goRight :: Zipper a -> Zipper a
goRight (LNode x y , ts) = (y, R x :ts)

goFullyRight :: Zipper a -> Zipper a
goFullyRight (LLeaf x , ts) = (LLeaf x , ts)
goFullyRight (LNode x y , ts) = goFullyRight (goRight (LNode x y , ts))

goUp :: Zipper a -> Zipper a
goUp (x, L y :ts) = (LNode x y, ts)
goUp (y, R x :ts) = (LNode x y, ts)

goToRoot :: Zipper a -> Zipper a
goToRoot (a, []) = (a, [])
goToRoot (a, ts) = goToRoot(goUp (a, ts))

increment :: Zipper Int -> Zipper Int
increment (LLeaf x , ts) = (LLeaf (x+1) , ts)


--Exercise 7
evenedges = [ (n,n+1) | n <-[0,2..998] ]
oddedges = [ (n, n `div` 5) | n <- [1,3..999] ]

graph = buildG (0,1000) (evenedges++oddedges)

isReachable :: Int -> Int -> Bool
isReachable n m = m `elem` reachable graph n


--Exercise 8
data GGraph a = GNode a (GGraph a) deriving Show

mkGraph :: [ (a, Int) ] -> [GGraph a]
mkGraph table = table'
 where table' = map (\(x,n) -> GNode x (table'!!n)) table

table = merge evenedges oddedges
  where merge (x:xs) ys = x : merge ys xs
        merge [] ys = ys

graphCD = mkGraph table

nextNode :: GGraph a -> GGraph a
nextNode (GNode a g) = g

nodeID :: GGraph a -> a
nodeID (GNode v g) = v

isReachableCD :: Int -> Int -> Bool
isReachableCD n m = isReachableCD' n m []
    where isReachableCD' n m visited | n == m = True
          isReachableCD' n m visited | n `elem` visited = False 
          isReachableCD' n m visited = isReachableCD' (nodeID $ nextNode (graphCD !! n)) m (n : visited)


--Exercise A6 (100% Accuracy)
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
putT v zi | (getNode left /= Leaf) && v < getVal zi = putT v left
            | (getNode right /= Leaf) && v > getVal zi = putT v right
            | (getNode left == Leaf) && v < getVal zi = putIn v left
            | otherwise = putIn v right
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