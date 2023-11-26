module TestZ3_test1 where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

-- script :: Z3 Int
script = do
  int_  <- (mkIntSort :: Z3 Sort)
  bool_ <- mkBoolSort
  bool_array <- mkArraySort int_ bool_
  -- declare an array a:
  a_  <- mkFreshConst "a" bool_array

  i_  <- mkFreshIntVar "i"
  i'    <- toApp i_

  a_i <- mkSelect a_ i_

  _0 <- mkInteger 0
  _4 <- mkInteger 4
  c1 <- mkLe _0 i_
  c2 <- mkLt i_ _4
  body <- mkAnd [c1,c2,a_i]

  f  <- mkExistsConst [] [i'] body

  assert f
  -- SAT-check and get the model:
  (sat, Just m) <- solverCheckAndGetModel
  str <- showModel m
  return (sat,str)

main :: IO ()
main = do
   r <- evalZ3 script
   putStrLn . show $ r
