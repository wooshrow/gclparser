{-
   This module provides a function to inject artificial errors in
   a target GCL program. Given a program P, the function mutateProgram P
   produces a list of 'mutants'. Each mutant is a pair of (mty,P') where
   P' is a copy of P, which just one mutation/change in its code
   (e.g. some expression "x<n" is changed to "x<=n"). This mutation is
   meant to simulate some error made by a programmer. Importantly, note
   that each mutant only has ONE change.
   The "mty" part indicates the type of mutation, e.g. ROR_LTE_LT means
   that the mutation injected changes some "<=" to "<".

   You can use mutants to test the strength of your test-suite, or your
   verification. Since a mutant represents an error, a good test suite
   should be able to detect it (that it is an erronous instance of P).
   We then say that your test-suite (or verification) "kills" the mutant.
   Given a bunch of generated mutants, ideally your test-suite should
   be able to kill them all.

   The above use of mutants to test your test-suite is also called
   "mutation test" (so... note that in mutation testing you don't test
   P, but rather, you test P's test-suite).

   It is important for you to realize the following:

   (1) The implementation below does not actually check if each mutation
   is semantically unique. That is, that each mutant P' would behave
   differently than other generated mutants, as well as the original P.
   Proving this is theoretically undecidable.

   (2) As a consequence of (1), a mutant P' may turn out to be
   semantically non-mutant. That is, P' turn out to be semantically
   equivalent to P. Such a mutant P' is called an "equivalent mutant".
   Proving that a mutant is an equivalent mutant is also theoretically
   undecidable.
   This also implies, that if your test-suite fails to kill some mutant,
   there is no general method to decide if the failure is due to
   your test-suite's incompleteness, or duNe to the mutant being an
   equivalent mutant. This implies that you will have to inspect the
   case mannually.

-}
module MuGCL
where

import GCLParser.GCLDatatype
import GCLParser.Parser
import GCLParser.PrettyPrint

data MutationType = NO_MUTATION |
     -- mutations on <= < > >=
     ROR_LTE_LT | ROR_LTE_GTE | ROR_LT_LTE | ROR_LT_GT |
     ROR_GTE_GT | ROR_GTE_LTE | ROR_GT_GTE | ROR_GT_LT |

     -- mutaions on literals 1,2... won't do them, some like
     -- changing 1 to 0 is already covered by e.g. ADD_PLUS_DROP_LEFT/RIGHT
     -- LIT_ONE_ZERO |

     --  mutations on + - / *
     AOR_PLUS_DROP_LEFT | AOR_PLUS_DROP_RIGHT | AOR_PLUS_MINUS |
     AOR_MINUS_DROP_RIGHT | AOR_MINUS_ROTATE | AOR_MINUS_PLUS |
     AOR_TIMES_DROP_LEFT | AOR_TIMES_DROP_RIGHT |
     AOR_DIV_DROP_RIGHT | AOR_DIV_ROTATE |

     -- mutations on && || IMP
     LOR_AND_DROP_LEFT | LOR_AND_DROP_RIGHT | LOR_AND_OR |
     LOR_OR_DROP_LEFT  | LOR_OR_DROP_RIGHT   | LOR_OR_AND |
     LOR_IMP_DROP_LEFT  | LOR_IMP_DROP_RIGHT | LOR_IMP_ROTATE |

     -- mutation on Equal and Alias
     EOR_EQUAL_NEGATE | EOR_ALIAS_NEGATE |

     -- mutation on ~
     NOT_DROP |

     -- mutation on if-then-else
     ITE_DROP_ELSE |

     -- mutation on TryCatch
     TRYCATCH_DROP_CATCH

     deriving (Eq,Show)


($>) :: (x->c)->(a,x)->(a,c)
f $> (a,x) = (a, f x)

mutateExpr :: Expr -> [(MutationType,Expr)]
mutateExpr expr = filter (\m -> fst m /= NO_MUTATION) $ mutate expr
   where
   mutate expr = case expr of
      Var v   -> [(NO_MUTATION, expr)]
      LitI x  -> [(NO_MUTATION, expr)]
      LitB x  -> [(NO_MUTATION, expr)]
      LitNull -> [(NO_MUTATION, expr)]
      Parens e ->  [ Parens $> e' | e' <- mutate e]
      ArrayElem a i ->
         let
         group1 = map ((\a_ -> ArrayElem a_ i) $>) $ mutate a
         group2 = map ((\i_ -> ArrayElem a i_) $>) $ mutate i
         in
         group1 ++ group2

      OpNeg e -> (NOT_DROP, e) : map (OpNeg $>) (mutate e)

      BinopExpr op e1 e2 ->
         let
         group1 = map ((\e1_ -> BinopExpr op e1_ e2) $>) $ mutate e1
         group2 = map ((\e2_ -> BinopExpr op e1 e2_) $>) $ mutate e2
         toplevel_mutants = case op of
            LessThan ->
              let
              m1 = (ROR_LT_LTE, BinopExpr LessThanEqual e1 e2)
              m2 = (ROR_LT_GT , BinopExpr GreaterThan e1 e2)
              in
              [m1,m2]
            LessThanEqual ->
              let
              m1 = (ROR_LTE_LT , BinopExpr LessThan e1 e2)
              m2 = (ROR_LTE_GTE, BinopExpr GreaterThanEqual e1 e2)
              in
              [m1,m2]
            GreaterThan ->
              let
              m1 = (ROR_GT_LT, BinopExpr LessThan e1 e2)
              m2 = (ROR_GT_GTE , BinopExpr GreaterThanEqual e1 e2)
              in
              [m1,m2]
            GreaterThanEqual ->
              let
              m1 = (ROR_GTE_GT, BinopExpr GreaterThan e1 e2)
              m2 = (ROR_GTE_LTE , BinopExpr LessThanEqual e1 e2)
              in
              [m1,m2]
            Plus ->
              let
              m1 = (AOR_PLUS_DROP_LEFT, e2)
              m2 = (AOR_PLUS_DROP_RIGHT, e1)
              m3 = (AOR_PLUS_MINUS, BinopExpr Minus e1 e2)
              in
              [m1,m2,m3]
            Minus ->
              let
              m1 = (AOR_MINUS_DROP_RIGHT, e1)
              m2 = (AOR_MINUS_ROTATE, BinopExpr Minus e2 e1)
              m3 = (AOR_MINUS_PLUS, BinopExpr Plus e1 e2)
              in
              [m1,m2,m3]
            Multiply ->
              let
              m1 = (AOR_TIMES_DROP_LEFT, e2)
              m2 = (AOR_TIMES_DROP_RIGHT, e1)
              in
              [m1,m2]
            Divide ->
              let
              m1 = (AOR_DIV_DROP_RIGHT, e1)
              m2 = (AOR_DIV_ROTATE, BinopExpr Divide e2 e1)
              in
              [m1,m2]
            And ->
              let
              m1 = (LOR_AND_DROP_RIGHT, e1)
              m2 = (LOR_AND_DROP_LEFT, e2)
              m3 = (LOR_AND_OR, BinopExpr Or e1 e2)
              in
              [m1,m2,m3]
            Or ->
              let
              m1 = (LOR_OR_DROP_RIGHT, e1)
              m2 = (LOR_OR_DROP_LEFT, e2)
              m3 = (LOR_OR_AND, BinopExpr And e1 e2)
              in
              [m1,m2,m3]
            Implication ->
              let
              m1 = (LOR_IMP_DROP_RIGHT, e1)
              m2 = (LOR_IMP_DROP_LEFT, e2)
              m3 = (LOR_IMP_ROTATE, BinopExpr Implication e2 e1)
              in
              [m1,m2,m3]
            Equal ->
              let
              m1 = (EOR_EQUAL_NEGATE, OpNeg $ BinopExpr Equal e1 e2)
              in
              [m1]
            Alias ->
              let
              m1 = (EOR_ALIAS_NEGATE, OpNeg $ BinopExpr Alias e1 e2)
              in
              [m1]
         in
         toplevel_mutants ++ group1 ++ group2
      --
      -- We are not going to mutate Forall and Exists, because
      -- they should only appear in Assume and Assert:
      Forall _ _ -> [(NO_MUTATION, expr)]
      Exists _ _ -> [(NO_MUTATION, expr)]

      SizeOf a -> map (SizeOf $>) $ mutate a

      --
      -- Not going to mutate RepBy and Cond as they should only
      -- appear as intermediate expressions during verification
      --
      RepBy _ _ _ -> [(NO_MUTATION, expr)]
      Cond  _ _ _ -> [(NO_MUTATION, expr)]

      NewStore e -> map (NewStore $>) $ mutate e

      Dereference p -> [(NO_MUTATION, expr)]

mutateStmt :: Stmt -> [(MutationType,Stmt)]
mutateStmt stmt = filter (\m -> fst m /= NO_MUTATION) $ mutate stmt
  where
  mutate stmt = case stmt of
     Skip     -> [(NO_MUTATION, stmt)]
     Assert e -> [(NO_MUTATION, stmt)]
     Assume e -> [(NO_MUTATION, stmt)]
     Assign  x e    -> map (Assign x $>) $ mutateExpr e
     DrefAssign x e -> map (DrefAssign x $>) $ mutateExpr e
     AAssign x i e -> map ((\i_ -> AAssign x i_ e) $>) (mutateExpr i)
                      ++
                      map ((\e_ -> AAssign x i e_) $>) (mutateExpr e)

     -- we will not drop parts of Seq as that might remove some Asserts
     Seq stmt1 stmt2 -> map ((\s1_ -> Seq s1_ stmt2) $>) (mutate stmt1)
                        ++
                        map ((\s2_ -> Seq stmt1 s2_) $>) (mutate stmt2)

     IfThenElse g sThen sElse ->
         let
         group1 =  map ((\g_  -> IfThenElse g_ sThen sElse) $>) $ mutateExpr g
         group2 =  map ((\s1_ -> IfThenElse g s1_ sElse) $>) $ mutate sThen
         group3 =  map ((\s2_ -> IfThenElse g sThen s2_) $>) $ mutate sElse
         m1 = case sElse of
                Skip -> []
                _    -> [(ITE_DROP_ELSE, IfThenElse g sThen Skip)]
         in
         m1 ++ group1 ++ group2 ++ group3

     While g body -> map ((\g_ -> While g_ body) $>) (mutateExpr g)
                     ++
                     map ((\body_ -> While g body_) $>) (mutate body)

     Block vars body -> map (Block vars $>) $ mutate body

     TryCatch exc body handler ->
        let
        group1 = map ((\body_ -> TryCatch exc body_ handler) $>) $ mutate body
        group2 = map ((\h_ -> TryCatch exc body h_) $>) $ mutate handler
        m1 = (TRYCATCH_DROP_CATCH, body)
        in
        m1 : group1 ++ group2


mutateProgram :: Program -> [(MutationType,Program)]
mutateProgram (Program name inputParams outputParams body)
   =
   map (Program name inputParams outputParams $>) $ mutateStmt body


-- tests
test_ = do
   gcl <- parseGCLfile "../examples/benchmark/bsort.gcl"
   let (Right prg) = gcl
   putStrLn . ppProgram2String $ prg
   putStrLn ""
   putStrLn ("# generated mutants " ++ (show . length . mutateProgram $ prg))
   putStrLn ("** mutants " ++ (show . map fst . mutateProgram $ prg))
