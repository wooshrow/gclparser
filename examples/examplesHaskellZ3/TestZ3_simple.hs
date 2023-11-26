module TestZ3_simple where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad


--
-- A simple example to construct the formula a>b && b>0, and then
-- we will SAT-check it.
--

-- script has type Z3 (...)
script = do
  a <- mkFreshIntVar "a"
  b <- mkFreshIntVar "b"
  _0 <- mkInteger 0
  t1 <- mkGt a b
  t2 <- mkGt b _0
  f  <- mkAnd [t1,t2]

  -- a > b && b > 0 to z3 state:
  assert f
  -- SAT-check and get the model:
  (sat,Just m) <- solverCheckAndGetModel
  aVal <- evalInt m a
  bVal <- evalInt m b
  return(sat,aVal,bVal)


main :: IO ()
main = do
   r <- evalZ3 script
   putStrLn . show $ r
