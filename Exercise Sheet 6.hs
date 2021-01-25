--Exercise 1
import Data.List
type Environment = [(String,Expr)]
data Expr = Var String | Lam String Expr | App Expr Expr | Cl String Expr Environment deriving (Eq,Show,Read)

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] =  Nothing
lookup key ((x,y):ps)  | key == x =  Just y
                       | otherwise = lookup key ps


--Exercise 2
data Frame = AppHole Expr | HoleApp Expr Environment
type Kontinuation = [Frame]
type Configuration = ( Expr , Environment, Kontinuation )


--Exercise 3
update :: Environment -> String -> Expr -> Environment
update env x e = (x,e) : env

unpack :: Expr -> (Expr,Environment)
unpack (Cl x e env1) = (Lam x e , env1)
unpack _ = error "Cannont unpack closure"

checkLookup :: Maybe Expr -> Expr
checkLookup Nothing = error "Unbound variable found"
checkLookup (Just e) = e

isValue :: Expr -> Bool
isValue Cl _ _ _ = True
isValue _ = False

--Small step evaluation function
eval1 :: Configuration -> Configuration 
-- Rule R1
eval1 (Var x,env,k) = (e',env',k) 
      where (e',env') = unpack $ checkLookup $ lookup x env
-- Rule R2
eval1 (App e1 e2,env,k) = (e1,env, HoleApp e2 env : k)
-- Rule R3
eval1 (Lam x e,env,k) = (Cl x e env, env, k)
-- Rule R4
eval1 (w,env1,(HoleApp e env2):k ) | isValue w = (e, env2, AppHole w : k)
-- Rule R5
eval1 (w,env1,(AppHole (Cl x e env2) ) : k ) | isValue w  = (e, update env2 x w, k)
-- Rule for terminated evaluations
eval1 c@(Cl {},_,[]) = c
-- Rule for runtime errors, if no cases above match then something has gone wrong
eval1 _ = error "Evaluation Error"


--Exercise 4
evalLoop :: Expr -> Expr
evalLoop e =   fst $ unpack $ evalLoop' (e,[],[])
   where evalLoop' :: Configuration -> Expr
         evalLoop' (e,env,k) = case eval1 (e,env,k) of
                                    r@(w,_,[]) -> if isValue w then w else evalLoop' r
                                    c -> evalLoop' c


--Exercise 5
testTerm :: Expr
testTerm = read "App (Lam \"v\"(App (App (Var \"v\") (Lam \"z\" (Var \"z\")))(App (Lam \"v\" (App (App (Var \"v\") (Lam \"x\" (Lam \"y\" (Var \"x\")))) (Lam \"z\" (Var \"z\")))) (Lam \"x\" (Lam \"y\" (Var \"x\")))))) (Lam \"x\" (Lam \"y\" (Var \"y\")))"


--Exercise 6
data Expr = Var String | Zero | Succ Expr | Lam String Expr | App Expr Expr | Cl String Expr Environment deriving (Eq,Show,Read)
data Frame = AppHole Expr | HoleApp Expr Environment | SuccHole

isValue :: Expr -> Bool
isValue Zero = True
isValue (Succ e) = isValue e
isValue Cl _ _ _ = True
isValue _ = False

unpack :: Expr -> (Expr,Environment)
unpack Zero = (Zero,[])
unpack (Succ e) = let (e',env) = unpack e in (Succ e',env) 
unpack (Cl x e env) = (Lam x e, env)
unpack _ = error "Cannot unpack closure value"

eval1 :: Configuration -> Configuration
--R1:  x|E1|K ⟼ λx -> e| E2 | K where lookup x in E1 is cl(λx -> e,E2)
eval1 (Var x,env1,k) = (e,env2,k)
   where (e,env2) = unpack $ checkLookup $ lookup x env1

--R2:  e1 e2| E| K ⟼ e1 | E | ([-] e2 E):K
eval1 (App e1 e2,env,k) = ( e1 , env , HoleApp e2 env : k )

--R2': Succ e | E | K ⟼ e | E | (SuccHole) : K
eval1 (Succ e,env,k) = (e,env,k)

--R3:  λx -> e | E | K ⟼ cl(λx -> e,E) | E | K
eval1 (Lam x e,env,k) = (Cl x e env,env,k)

--R4: W | E1 | ([-] e E2 : K) ⟼ e | E2 | (W[-]:K)
eval1 (w, env1, HoleApp e env2 : k ) | isValue w = (e, env2, AppHole w : k)

--R4' : W | E | (SuccHole) : K ⟼ Succ W | E | K
eval1 (w,env,SuccHole:k) | isValue w = (Succ w,env,k)

--R5: W | E1 | (cl(λx -> e, E2)[-]:K) ⟼ e | E2 [x:=W]| K
eval1 (w,env1, AppHole (Cl x e env2) : k ) | isValue w = (e , update env2 x w , k)

eval1 c@(w,_,[]) | isValue w = c
eval1 _ = error "Evaluation Error"


--Exercise A7 (100% Accuracy)
data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Read)
type Stack = [Maybe Int]
type SMProg = [Instruction]

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


--Exercise A8 (100% Accuracy)
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers s = r where
  
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


--Exercise A9 (100% Accuracy)
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