module Z3ProverExample where

import Z3.Monad
-----------------------------------------------------------------------

-- a (Z3) structure representing the fomula "x>1"
f1 :: Z3 AST
f1 = do
    xname <- mkStringSymbol "x"
    x <- mkIntVar xname
    one <- mkIntNum 1
    x `mkGt` one

-- a (Z3) structure representing the fomula "x>0"
f0 :: Z3 AST
f0 = do
    xname <- mkStringSymbol "x"
    x <- mkIntVar xname
    zero <- mkIntNum 0
    x `mkGt` zero

-- a (Z3) structure representing the fomula "x>1  ==>  x>0"
f2 :: Z3 AST
f2 = do
    -- this won't work:
    -- f <- f1 `mkImplies` f0
    -- The type "Z3" is a Monad, so we need to get the fomulas from
    -- the monad first:

    -- get f0; do the same with f1:
    f0_ <- f0
    f1_ <- f1
    -- form f1 ==> f0
    f1_ `mkImplies` f0_


-- An example of checking if a formula is satisfiable.
-- We will use the formula f1 (x>1) as an example. This
-- formula is satisfiable, so Z3 should come with the same
-- conclusion:
testSAT :: IO()
testSAT =
  let
  -- Construct a Z3 checker. It will "assert" the formula f1, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 Result
  checker = do
        f2_ <- f2
        assert f2_                   -- asserting the formula to check
        (verdict,model) <- getModel  -- checking if the formula is satisfiable
        return verdict               -- returning the verdict Sat or Unsat
  in
  -- Next, we run the checker. The we check if the returned verdict is Sat,
  -- and if so we print some Yay-message.
  do verdict <- evalZ3 checker
     if verdict == Sat
        then print "The formula f1 is satisfiable."
        else print "The formula f1 is UNsatisfiable."


-- An example of checking if a formula is valid.
-- We will use the formula f2 (x>1 ==> x>0) as an example. This
-- formula is valid, so Z3 should come with the same
-- conclusion. Because the API provided by Z3-binding can only
-- check satisfiability, we will instead check if "not f2" is
-- satisfiable. If it is, then f2 is not valid. If "not f2" is
-- unsatisfiable, then f2 is valid.
testVALID :: IO()
testVALID =
  let
  -- Construct a Z3 checker. It will "assert" the formula not-f2, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 Result
  checker = do
    f2_ <- f2
    f <- mkNot f2_
    assert f                         -- asserting the formula to check
    (verdict,model) <- getModel  -- checking if the formula is satisfiable
    return verdict               -- returning the verdict Sat or Unsat
  in
  -- Next, we run the checker. The we check if the returned verdict is Unsat,
  -- and if so we print some Yay-message.
  do verdict <- evalZ3 checker
     if verdict == Unsat
        then print "The formula f2 is valid."
        else print "The formula f2 is invalid."