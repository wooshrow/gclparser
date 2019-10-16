-- Contain some simple examples showing how to write Haskell functions that 
-- walk a datatype value to compute something.

module ExamplesOfSemanticFunction where

import GCLParser.GCLDatatype

-- a function to collect all variables in a given expression. Expr is a dataype defined
-- in the module GCLParser.GCLDatatype, representing GCL expressions.
collectAllVariables :: Expr -> [String]
collectAllVariables (Var x)    = [x]
collectAllVariables (LitI _)   = []
collectAllVariables (LitB _)   = []
collectAllVariables LitNull    = []
collectAllVariables (Parens e) = collectAllVariables e
collectAllVariables (ArrayElem a e) = collectAllVariables a ++ collectAllVariables e
collectAllVariables (OpNeg e)       = collectAllVariables e
collectAllVariables (BinopExpr _ e1 e2) = collectAllVariables e1 ++ collectAllVariables e2
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
