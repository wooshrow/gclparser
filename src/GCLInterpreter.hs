-- ignore this, work in progress
module GCLInterpreter
    (
    )
where

import GCLParser.GCLDatatype

data Value = Bool Bool
           | Int Int
           | ArrayBool [Bool]
           | ArrayInt [Int]
           | Pointer String
           deriving (Eq,Show)

null_ = Pointer "null"


type State = [(String,Value)]

-- Update x in the given state to bind it to the given value.
-- If x does not exists in the state, it is added.
update :: String -> Value -> State -> State
update x val [] = [(x,val)]
update x val ((y,v) : state)
   | x==y       = (x,val) : state
   | otherwise  = (y,v) : update x val state

(<@>) :: State -> String -> Value
[] <@> x = error ("The variable " ++ x ++ " does not exist in the current state.")
((y,v) : state) <@> x
   | x==y = v
   | otherwise = state <@> x


remove :: String -> State -> State
remove x [] = []
remove x ((y,v) : state)
   | x==y = state
   | otherwise = (y,v) : remove x state

-- get the size of the largest array in the state
getLargestArraySize :: State -> Int
getLargestArraySize [] = 0
getLargestArraySize ((_,v) : state) = case v of
    ArrayBool a -> length a `max` getLargestArraySize state
    ArrayInt a  -> length a `max` getLargestArraySize state
    _           -> getLargestArraySize state

--
-- allocate a new object, containing the integer x, into the
-- given state. Return the ref-name pointing to this new object,
-- and the new state after adding the object.
--
allocateNewObject x [] =  (refname, update refname (Int x) [])
   where
   refname = "__p0"
allocateNewObject x state = (refname, update refname (Int x) state)
   where
   refname = "__p" ++ show n
   n = getIndexOfLastObj state + 1
   getIndexOfLastObj [] = -1
   getIndexOfLastObj ((v,_) : state) = i `max` getIndexOfLastObj state
       where
       i = if take 3 v == "__p" then read (drop 3 v) else -1
--
-- Bunch of operations on Value
--
intOp :: (Int->Int->Int) -> Value -> Value -> Value
intOp binop (Int i) (Int j) = Int (binop i j)
intOp _ v1 v2 = error ("Expecting integers in e1 op e2: " ++ show v1 ++ ", " ++ show v2)

numrelOp :: (Int->Int->Bool) -> Value -> Value -> Value
numrelOp binop (Int i) (Int j) = Bool (binop i j)
numrelOp _ v1 v2 = error ("Expecting integers in e1 op e2: " ++ show v1 ++ ", " ++ show v2)

boolOp :: (Bool->Bool->Bool) -> Value -> Value -> Value
boolOp binop (Bool b1) (Bool b2) = Bool (binop b1 b2)
boolOp _ v1 v2 = error ("Expecting booleans in e1 op e2: " ++ show v1 ++ ", " ++ show v2)

boolNot :: Value -> Value
boolNot (Bool b) = Bool (not b)
boolNot v = error ("Expecting a boolean in not e: " ++ show v)

equalOp :: Value -> Value -> Value
equalOp (Int i) (Int j)     = Bool (i==j)
equalOp (Bool b1) (Bool b2) = Bool (b1==b2)
equalOp (ArrayBool a1) (ArrayBool a2) = Bool (a1 == a2)
equalOp (ArrayInt a1)  (ArrayInt a2)  = Bool (a1 == a2)
equalOp (Pointer p1)   (Pointer p2)   = Bool (p1 == p2)
equalOp v1 v2 = error ("Incompatible value-types for = or == : " ++ show v1 ++ ", " ++ show v2)

arrayRead :: Value -> Value -> Value
arrayRead (ArrayBool ab) (Int i) = Bool (ab !! i)
arrayRead (ArrayInt a)   (Int i) = Int (a !! i)
arrayRead a i = error ("Incompatible value-types in a[i]: " ++ show a ++ ", " ++ show i)

valueToBool :: Value -> Bool
valueToBool (Bool b) = b
valueToBool e = error ("Expecting a boolean value: " ++ show e)



-- replaceing a varibale v (typically a bounded var) in an expression
-- with a concrete integer i.
instantiateVarWithInt :: String -> Int -> Expr -> Expr
instantiateVarWithInt vname i expr = case expr of
   Var x    -> if x==vname then LitI i else expr
   LitI _   ->  expr
   LitB _   ->  expr
   LitNull  -> expr
   Parens e -> Parens (instantiateVarWithInt vname i e)
   ArrayElem a e -> ArrayElem a (instantiateVarWithInt vname i e)
   OpNeg e    -> OpNeg (instantiateVarWithInt vname i e)
   BinopExpr op e1 e2 -> BinopExpr op (instantiateVarWithInt vname i e1) (instantiateVarWithInt vname i e2)
   Forall x e ->
      if x==vname then expr else Forall x (instantiateVarWithInt vname i e)
   Exists x e ->
      if (x==vname) then expr else Exists x (instantiateVarWithInt vname i e)
   SizeOf e   -> SizeOf (instantiateVarWithInt vname i e)
   NewStore e -> NewStore (instantiateVarWithInt vname i e)
   Dereference p -> expr



eval :: State -> Expr -> Value
eval state expr = case expr of
  Var x -> state <@> x
  LitI i ->  Int i
  LitB b -> Bool b
  LitNull ->  null_
  Parens e -> eval state  e
  ArrayElem a i -> arrayRead (eval state a) (eval state i)
  OpNeg e -> boolNot . eval state $ e
  BinopExpr op e1 e2 ->
     let
     v1 = eval state e1
     v2 = eval state e2
     in
     case op of
       And -> boolOp (&&) v1 v2
       Or  -> boolOp (||) v1 v2
       Implication -> boolOp (\a b -> not a || b) v1 v2
       LessThan -> numrelOp (<) v1 v2
       LessThanEqual -> numrelOp (<=) v1 v2
       GreaterThan -> numrelOp (>) v1 v2
       GreaterThanEqual -> numrelOp (>=) v1 v2
       Equal -> equalOp v1 v2
       Minus -> intOp (-) v1 v2
       Plus -> intOp (+) v1 v2
       Multiply -> intOp (*) v1 v2
       Divide -> intOp (div) v1 v2
       Alias -> equalOp v1 v2
  -- becareful, this implementation of Forall and Exists is unsound!
  -- it only quantifies over int in the range of 0 up-to the size of
  -- the largest array currently in the memory.
  Forall x body ->
     let
     n = getLargestArraySize state
     in
     Bool . and $ [ valueToBool . eval state $ (instantiateVarWithInt x i body) | i <- [0..n-1]]
  Exists x body ->
     let
     n = getLargestArraySize state
     in
     Bool . or $ [ valueToBool . eval state $ (instantiateVarWithInt x i body) | i <- [0..n-1]]
  SizeOf a -> case eval state a of
     ArrayBool a_ -> Int $ length a_
     ArrayInt  a_ -> Int $ length a_
     _            -> error ("Expecting an array: " ++ show a)
  --NewStore e -> case eval state e of
  --   Int x -> allocateNewObject

{-
      collectAllVariables (Forall _ e)    = collectAllVariables e
      collectAllVariables (Exists _ e)    = collectAllVariables e
      collectAllVariables (SizeOf a)      = collectAllVariables a
      collectAllVariables (RepBy a e1 e2) = collectAllVariables a ++ collectAllVariables e1 ++ collectAllVariables e2
      collectAllVariables (Cond g e1 e2)  = collectAllVariables g ++ collectAllVariables e1 ++ collectAllVariables e2
      collectAllVariables (NewStore e)    = collectAllVariables e
      collectAllVariables (Dereference x) = [x]

      -- a function to collect all free variables in a given expression.
      freeVariables :: Expr -> [String]
      freeVariables (Var x)    = [x]
      freeVariables (LitI _)   = []
      freeVariables (LitB _)   = []
      freeVariables LitNull    = []
      freeVariables (Parens e) = freeVariables e
      freeVariables (ArrayElem a e) = freeVariables a ++ freeVariables e
      freeVariables (OpNeg e)  = freeVariables e
      freeVariables (BinopExpr _ e1 e2) = freeVariables e1 ++ freeVariables e2

      freeVariables (Forall x e)    = filter not_x (freeVariables e)
          where
          not_x y = y /= x

      freeVariables (Exists x e)    = filter not_x (freeVariables e)
          where
          not_x y = y /= x

      freeVariables (SizeOf a)      = freeVariables a
      freeVariables (RepBy a e1 e2) = freeVariables a ++ freeVariables e1 ++ freeVariables e2
      freeVariables (Cond g e1 e2)  = freeVariables g ++ freeVariables e1 ++ freeVariables e2
      freeVariables (NewStore e)    = freeVariables e
      freeVariables (Dereference x) = [x]

-}
