-- Exercise 1
sequenceM :: Monad m => [ m a ] -> m [ a ]
sequenceM [] = return []
sequenceM (mx:mxs) = do x <- mx
                        xs <- sequenceM mxs
                        return (x:xs)


-- Exercise 2
type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [ KnightPos ]
moveKnight (c,r) = filter onBoard 
 [ (c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1),
   (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2) ]
  where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

inN :: Int -> KnightPos -> [KnightPos]
inN 0 start = return start
inN n start = inN (n-1) start >>= moveKnight

minMoves :: KnightPos -> KnightPos -> Int
minMoves start destination = head [ n | n <- [0..] , pos <- inN n start, pos == destination ]


-- Exercise 3
newtype ZipList a = Z [a] deriving (Eq,Show,Read)
  
instance Functor ZipList where
   -- fmap :: (a -> b) -> ZipList a -> ZipList b
   fmap g (Z xs) = Z ( map g xs )

instance Applicative ZipList where
   -- pure :: a -> ZipList a
   pure x = Z (repeat x)

   -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
   (Z gs) <*> (Z xs) = Z ( zipWith ($) gs xs )


-- Exercise 4
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Eq,Show,Read)

instance Functor Expr where
 -- fmap :: (a -> b) -> Expr a -> Expr b
 fmap g (Var x) = Var (g x)
 fmap g (Val n) = Val n
 fmap g (Add le re) = Add (fmap g le) (fmap g re)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var
  
  -- (<*>) :: Expr (a->b) -> Expr a -> Expr b
  (Var g) <*> (Var x) = Var (g x)
  mg <*> (Val n) = Val n
  mg <*> (Add le re) = Add (mg <*> le) (mg <*> re)
  (Val n) <*> (Var x) = Val n
  (Add mg mh) <*> e = Add ( mg <*> e) (mh <*> e)

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var x) >>= f = f x
  (Val n) >>= f = Val n
  (Add le re) >>= f = Add (le >>= f) (re >>= f)

subst :: Eq a => a -> Expr a -> Expr a -> Expr a
subst x e1 e2 = do v <- e2
                   if x == v then e1 else Var v


-- Exercise 5
data LTree a = Leaf a | Node (LTree a) (LTree a) deriving (Eq,Show,Read)

instance Functor LTree where
    -- fmap :: (a->b) -> LTree a -> LTree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node e1 e2) = Node (fmap g e1) (fmap g e2)

instance Applicative LTree where
    -- pure :: a -> LTree a
    pure = Leaf
    -- (<*>) :: LTree (a -> b) -> LTree a -> LTree b
    (Leaf g) <*> (Leaf x) = Leaf (g x)
    mg <*> Node e1 e2 = Node (mg <*> e1) (mg <*> e2)
    (Node mg mh) <*> e = Node (mg <*> e) (mh <*> e)

instance Monad LTree where
    -- (>>=) :: LTree a -> (a -> LTree b) -> LTree b
     (Leaf x) >>= f =  f x
     (Node e1 e2) >>= f = Node ( e1 >>= f ) (e2 >>= f)

type State = Int
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) = st

instance Functor ST where
  --fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x , s'))

instance Applicative ST where 
  -- pure :: a -> ST a
  pure x = S ( \s -> (x,s) )
  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S ( \s -> let (f,s')  = app stf s
                              (x,s'') = app stx s'
                          in (f x , s''))
 
instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S ( \s -> let (x,s') = app st s
                       in app (f x) s' )

fresh :: ST Int 
fresh = S ( \n -> (n,n+1))

dist :: LTree (ST a) -> ST (LTree a)
dist (Leaf s) = S (\n -> let (x,n') = app s n in (Leaf x,n'))
dist (Node l r) = S (\n -> let (lx,n') = app (dist l) n
                               (rx,n'') = app (dist r) n'
                           in (Node lx rx , n''))

relabel :: LTree a -> LTree Int
relabel t = fst $ app (aLabel t) 0

aLabel :: LTree a -> ST (LTree Int)
aLabel t =  dist $ t >>= (\x -> Leaf fresh)