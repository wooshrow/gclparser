{-
   A simple interpreter to execute a GCP program. The top level
   interface to execute the first procedure in a program is:

      callProgram gcl-procedure parameters

    You need to give it a state, which can usually be empty, and the values
    of all the input parameters of the procedure (in the right order).

    If the execution ends normally, you get a value of the form "Right res",
    where res is a list of return values.

    If the procedures ends by exeception you get "Left (msg,t)", where t is
    the last state just before the exeception is thrown, and msg is a
    message describing the kind of excpetion thrown.

    The execution can also crash (Haskell calls "error"), which means there
    is something else is wrong, e.g. the procedures tries to access a variable
    that is not in its current state, or tries to evaluate an expression which
    turns out to be type incorrect.

    Assume-statement will be skipped. Assert-statement will be checked, but
    quantifiers like Forall and Exists are not completely checked. The
    interpreter will only check instances of the bound-variable between 0
    and N, where N is the size of the largest array in the current state.

-}

module GCLInterpreter

where

import Data.List (find)
import GCLParser.GCLDatatype
import GCLParser.Parser
import GCLParser.PrettyPrint

-- Representation of values that result form evaluating expressions:
data Value = Bool Bool
           | Int Int
           | ArrayBool [Bool]
           | ArrayInt [Int]
           | Pointer String  -- to represent a pointer/reference. "Pointer p" .. p is a string representing a unique address pointing to some object.
           deriving (Eq,Show)

null_ :: Value
null_ = Pointer "null"


-- representing the state of an executing program:
type State = [(String,Value)]

-- ==========================================
-- Some utitlities to work on "State"
-- ==========================================

-- Update x in the given state to bind it to the given value.
-- If x does not exists in the state, it is added.
update :: String -> Value -> State -> State
update x val [] = [(x,val)]
update x val ((y,v) : state)
   | x==y       = (x,val) : state
   | otherwise  = (y,v) : update x val state

-- state <@> x gives the value of x in the state:
(<@>) :: State -> String -> Value
[] <@> x = error ("The variable " ++ x ++ " does not exist in the current state.")
((y,v) : state) <@> x
   | x==y = v
   | otherwise = state <@> x

--
-- Push a new variable, initialized to the given value, onto
-- the state.
pushVar :: String -> Value -> State -> State
pushVar x value state = (x,value) : state
--
-- Pop a variable from the state (removing its first instance)
--
popVar :: String -> State -> State
popVar _ [] = []
popVar x ((y,v) : state)
   | x==y = state
   | otherwise = (y,v) : popVar x state

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
allocateNewObject :: Int -> [(String,Value)] -> (String, State)
allocateNewObject x [] =  (refname, update refname (Int x) [])
   where
   refname = "__p0"
allocateNewObject x state = (refname, update refname (Int x) state)
   where
   refname = "__p" ++ show n
   n = getIndexOfLastObj state + 1 :: Int
   getIndexOfLastObj [] = -1
   getIndexOfLastObj ((v,_) : s) = i `max` getIndexOfLastObj s
       where
       i = if take 3 v == "__p" then read (drop 3 v) else -1

-- ==========================================
-- Bunch of operations on Value
-- ==========================================

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

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList k x (y:s)
   | k<=0  = x:s
   | otherwise = y : updateList (k-1) x s

arrayUpdate :: Int -> Value -> Value -> Value
arrayUpdate k (Bool x) (ArrayBool a) = ArrayBool (updateList k x a)
arrayUpdate k (Int x) (ArrayInt a)   = ArrayInt  (updateList k x a)
arrayUpdate _ x a = error ("Trying to update an array with an incompatible value: " ++ show a ++ ", " ++ show x)

arraySize :: Value -> Value
arraySize (ArrayBool ab) = Int $ length ab
arraySize (ArrayInt a)   = Int $ length a
arraySize a = error ("Expecting an array: " ++ show a)

valueToBool :: Value -> Bool
valueToBool (Bool b) = b
valueToBool e = error ("Expecting a boolean value: " ++ show e)

valueToInt :: Value -> Int
valueToInt (Int i) = i
valueToInt e = error ("Expecting an integer value: " ++ show e)

-- default initial value when declaring a local or return variable
initialValue :: Type -> Value
initialValue (PType PTInt) = Int 0
initialValue (PType PTBool) = Bool True
initialValue (AType PTInt) = ArrayInt []
initialValue (AType PTBool) = ArrayBool []
initialValue RefType = null_


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
   Dereference _ -> expr
   RepBy a idx e -> RepBy (instantiateVarWithInt vname i a) (instantiateVarWithInt vname i idx) (instantiateVarWithInt vname i e)
   Cond g e1 e2 -> RepBy (instantiateVarWithInt vname i g) (instantiateVarWithInt vname i e1) (instantiateVarWithInt vname i e2)

-- ==========================================
-- Eval and Exec functions
-- ==========================================

--
-- Evaluate an expression on the given state. It returns Right v, if
-- the expression can be evaluated to a value v.
-- Else, if an exeception like division-by-0 is encountered, then
-- Left msg is returned, where msg is a message describing the problem.
--
eval :: State -> Expr -> Either String Value
eval state expr = case expr of
  Var x  -> Right $ state <@> x
  LitI i -> Right $ Int i
  LitB b -> Right $ Bool b
  LitNull ->  Right $ null_
  Parens e -> eval state  e

  ArrayElem a i -> do
    a_ <- eval state a
    i_ <- valueToInt <$> eval state i
    let Int n_ = arraySize a_
    if 0<=i_ && i_ < n_
       then return $ arrayRead a_ (Int i_)
       else Left ("EXC2: illegal array index: " ++ show expr)

  OpNeg e -> do
     e_ <- eval state e
     return $ boolNot e_

  BinopExpr op e1 e2 -> do
     v1 <- eval state e1
     v2 <- eval state e2
     case op of
        Divide ->
            let Int i = v2
            in if i==0 then Left "EXC1: division by 0"
                       else return $ intOp (div) v1 v2
        And -> return $ boolOp (&&) v1 v2
        Or  -> return $ boolOp (||) v1 v2
        Implication -> return $ boolOp (\a b -> not a || b) v1 v2
        LessThan -> return $ numrelOp (<) v1 v2
        LessThanEqual -> return $ numrelOp (<=) v1 v2
        GreaterThan -> return $ numrelOp (>) v1 v2
        GreaterThanEqual -> return $ numrelOp (>=) v1 v2
        Equal -> return $ equalOp v1 v2
        Minus -> return $ intOp (-) v1 v2
        Plus -> return $ intOp (+) v1 v2
        Multiply -> return $ intOp (*) v1 v2
        Alias -> return $ equalOp v1 v2
  -- becareful, this implementation of Forall and Exists is unsound!
  -- it only quantifies over int in the range of 0 up-to the size of
  -- the largest array currently in the memory.
  Forall x body -> do
     let n = getLargestArraySize state
     values <- sequence [ eval state $ (instantiateVarWithInt x i body) | i <- [0..n-1]]
     return . Bool . and $ map valueToBool values
  Exists x body -> do
     let n = getLargestArraySize state
     values <- sequence [ eval state $ (instantiateVarWithInt x i body) | i <- [0..n-1]]
     return . Bool . or $ map valueToBool values
  SizeOf a -> do
     a_ <- eval state a
     return $ case a_ of
        ArrayBool a__ -> Int $ length a__
        ArrayInt  a__ -> Int $ length a__
        _             -> error ("Expecting an array: " ++ show a)

  Dereference p ->
     let
     Pointer ref = state <@> p
     value = state <@> ref
     in Right value
  -- this will be handled at the assignment level, since it can only occur
  -- in the form of x := new(e)
  NewStore _  -> undefined
  -- these should not occur (only produced as intermediate expr during
  -- wlp calculation):
  RepBy _ _ _ -> undefined
  Cond _ _ _  -> undefined
  --   Int x -> allocateNewObject

-- lifted eval
eval_ :: State -> Expr -> Either (String,State) Value
eval_ state expr = addStateToError state (eval state expr)

addStateToError :: State -> Either e a -> Either (e,State) a
addStateToError state = either (\e -> Left (e,state)) Right

dropStateFromError :: Either (e,State) a -> Either e a
dropStateFromError = either (Left . fst) Right



-- examples
s0__ :: [(String, Value)]
s0__ = [
  ("i", Int 2),
  ("k", Int 0),
  ("b", Bool False),
  ("u", Pointer "__p0"),
  ("a", ArrayBool [True,False,False]),
  ("__p0",Int 99)
  ]

--
-- This function would execute a statement on the given state.
-- If the execution is successful, the function returns Right t,
-- where t is the resulting state.
-- The execution might also aborts because of an exception was
-- thrown. In this case the function returns Left msg t, where
-- msg is some string describing the thrown exception, and t
-- is the state just before the exception was thrown.
-- Finally, an execution may fail to terminate, if the statement
-- itself contains a non-terminating loop.
--
exec :: [Procedure] -> State -> Stmt -> Either (String,State) State
exec procs state stmt = case stmt of
   Skip -> return state
   -- assume is ignored:
   Assume _ -> return state

   Assert p -> do
       ok <- valueToBool <$> eval_ state p
       if ok
          then return state
          else error ("Assertion violation: " ++ show stmt)

   -- v := new(10)
   Assign var (NewStore expr) -> do
        i <- valueToInt <$> eval_ state expr
        let (refToNewObj,state') = allocateNewObject i state
        let state'' = update var (Pointer refToNewObj) state'
        return state''

   -- ordinary assignment v := e
   Assign var expr -> do
        value <- eval_ state expr
        let state' = update var value state
        return state'

   DrefAssign var expr -> do
        value <- eval_ state expr
        let state' = update var value state
        return state'

   AAssign  a index expr -> do
        i <- valueToInt <$> eval_ state index
        let array = state <@> a
        let Int n = arraySize array
        if 0<=i && i<n
           then do
                e <- eval_ state expr
                let array' = arrayUpdate i e array
                let state' = update a array' state
                return state'
           else Left ("EXC2: illegal array index: " ++ show stmt, state)

   Call vars f args ->
       case find ((== f) . name) procs of
          Nothing -> error ("Calling undefined procedure: " ++ f)
          Just proc -> do
            evaluatedArgs <- traverse (eval_ state) args
            results <- addStateToError state $ callProcedure procs proc evaluatedArgs
            if length results /= length vars
              then
                error $
                  f ++ " returned " ++ show (length results) ++ " arguments,"
                  ++ " expected " ++ show (length vars)
              else
                return $ foldr (\(var, value) -> update var value) state (zip vars results)

   Seq stmt1 stmt2 -> do
       intermediateState <- exec procs state stmt1
       exec procs intermediateState stmt2

   IfThenElse guard stmtThen stmtElse -> do
       g <- valueToBool <$> eval_ state guard
       if g then exec procs state stmtThen
            else exec procs state stmtElse

   While guard body -> do
       g <- valueToBool <$> eval_ state guard
       if g then exec procs state (Seq body stmt)
            else return state

   TryCatch exc body handler ->
       let
       -- add exc as a new loc-var
       state1 = pushVar exc (Int 0) state
       in
       case exec procs state1 body of
          Right state2 -> return $ popVar exc state2
          Left (msg,state2) ->
            let
            state3 = case take 4 msg of
                          "EXC1" -> update exc (Int 1) state2
                          "EXC2" -> update exc (Int 2) state2
                          _      -> update exc (Int 9) state2
            in
            case exec procs state3 handler of
                Right state4      -> Right $ popVar exc state4
                Left (msg',state4) -> Left (msg', popVar exc state4)


   Block vardecls body ->
      let
      varnames = [ name | VarDeclaration name _ty <- vardecls]
      -- allocate the declared vars into the state:
      state2 = foldr (\(VarDeclaration name ty) state_ -> pushVar name (initialValue ty) state_)
                     state
                     vardecls

      in
      -- execute the body, then clean-up the vars we just added:
      case exec procs state2 body of
          Right state3      -> Right $ foldr (\v state_ -> popVar v state_) state3  varnames
          Left (msg,state3) -> Left (msg, foldr (\v state_ -> popVar v state_) state3  varnames)

-- Just executes the first procedure of the program.
execProgram :: Program -> State -> Either (String,State) State
execProgram (Program procs) state = execProcedure procs state (head procs)

execProcedure :: [Procedure] -> State -> Procedure -> Either (String,State) State
execProcedure procs state p = exec procs state (stmt p)

-- Just calls the first procedure of the program.
callProgram :: Program -> [Value] -> Either String [Value]
callProgram (Program procs) = callProcedure procs (head procs)

-- A function to execute a procedure on  given state. For example,
-- consider a procedure P(x | y) body, where x is an input parameter,
-- and y is an output parameter. To execute P you need to give it
-- a state that contains x and y as variables, along with their initial
-- values.
-- Just like the execution statements, the result of executing
-- a procedure is either Right t, where t is the resulting state.
-- The execution might also aborts because of an exception was
-- thrown. In this case the function returns Left msg, where
-- msg is some string describing the thrown exception.
--
callProcedure :: [Procedure] -> Procedure -> [Value] -> Either String [Value]
callProcedure _ (Procedure name inputVars _ _ _ _) args
  | length args /= length inputVars =
      error $
        "expected " ++ show (length inputVars) ++ " arguments to " ++ name
        ++ ", got " ++ show (length args)
callProcedure procs (Procedure name inputVars outputVars pre post stmt) args =
  let
  inputParamNames = [ var | VarDeclaration var _ <- inputVars]
  outputParamNames = [ var | VarDeclaration var _ <- outputVars]
  input = zip inputParamNames args
  output = [ (var, initialValue ty) | VarDeclaration var ty <- outputVars]
  in do
    checkPrecondition input
    state' <- dropStateFromError $ exec procs (input ++ output) stmt
    let outputResults = map (\v -> (v, getVar v state')) outputParamNames
    checkPostcondition (input ++ outputResults)
    return (map snd outputResults)

  where
    checkPrecondition state = do
      ok <- fmap valueToBool <$> traverse (eval state) pre
      case ok of
        Just False -> error ("Precondition violation when calling " ++ name ++ ": " ++ show state)
        _ -> return ()

    checkPostcondition state = do
      ok <- fmap valueToBool <$> traverse (eval state) post
      case ok of
        Just False -> error ("Postcondition violation in " ++ name ++ ": " ++ show state)
        _ -> return ()

    getVar v s = case lookup v s of
      Just value -> value
      Nothing -> error ("function " ++ name ++ " did not return " ++ v)

-- tests
test_ :: IO ()
test_ = do
   gcl <- parseGCLfile "../examples/benchmark/bsort.gcl"
   let (Right prg) = gcl
   putStrLn . ppProgram2String $ prg
   let state1 = [("a", ArrayInt [4,1,3,7,0]),
                 ("b", ArrayInt [0,0,0,0,0]),
                 ("r", Int 99),
                 ("i",Int 0), ("j",Int 4),
                 ("x",Int 0), ("y",Int 4), ("z",Int 99)
                 ]
   let state2 = execProgram prg state1
   putStrLn . show $ state2
