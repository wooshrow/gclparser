module TestZ3_test2 where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

-- script :: Z3 Int
script = do
  int_  <- (mkIntSort :: Z3 Sort)

  i_  <- mkFreshIntVar "i"
  i'  <- toApp i_

  _2 <- mkInteger 2
  body <- mkEq i_ _2

  f0  <- mkExistsConst [] [i'] body

  f <- mkNot f0

  assert f
  -- SAT-check and get the model:
  (sat, model) <- solverCheckAndGetModel
  str <- case model of
           Nothing -> return "No model"
           Just m  -> showModel m

  return (sat,str)

main :: IO ()
main = do
   r <- evalZ3 script
   putStrLn . show $ r
