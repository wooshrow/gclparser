module GCLParser.GCLAlgebra where

import GCLParser.GCLDatatype

type GCLProgramAlgebra p s e =
  (String -> [VarDeclaration] -> [VarDeclaration] -> s -> p,
   GCLStmtAlgebra s e)

type GCLStmtAlgebra s e =
    (s,
     e -> s,
     e -> s,
     String -> e -> s,
     String -> e -> e -> s,
     String -> e -> s,
     s -> s -> s,
     e -> s -> s -> s,
     e -> s -> s,
     [VarDeclaration] -> s -> s,
     String -> s -> s -> s,
     GCLExprAlgebra e)

type GCLExprAlgebra e =
    (String -> e,
     Int -> e,
     Bool -> e,
     e,
     e -> e,
     e -> e -> e,
     e -> e,
     BinOp -> e -> e -> e,
     String -> e -> e,
     e -> e,
     e -> e -> e -> e,
     e -> e -> e -> e,
     e -> e,
     String -> e)
     
foldGCLProgram :: GCLProgramAlgebra p s e -> Program -> p
foldGCLProgram (f1, f2) = fProgram
    where
      fProgram (Program a b c d) = f1 a b c (fStmt d)
      fStmt = foldGCLStmt f2

foldGCLStmt :: GCLStmtAlgebra s e -> Stmt -> s
foldGCLStmt (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) = fStmt
    where
      fStmt Skip = f1
      fStmt (Assert a) = f2 (fExpr a)
      fStmt (Assume a) = f3 (fExpr a)
      fStmt (Assign a b) = f4 a (fExpr b)
      fStmt (AAssign a b c) = f5 a (fExpr b) (fExpr c)
      fStmt (DrefAssign a b) = f6 a (fExpr b)
      fStmt (Seq a b) = f7 (fStmt a) (fStmt b)
      fStmt (IfThenElse a b c) = f8 (fExpr a) (fStmt b) (fStmt c)
      fStmt (While a b) = f9 (fExpr a) (fStmt b)
      fStmt (Block a b) = f10 a (fStmt b)
      fStmt (TryCatch a b c) = f11 a (fStmt b) (fStmt c)
      fExpr = foldGCLExpr f12

foldGCLExpr :: GCLExprAlgebra e -> Expr -> e
foldGCLExpr (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) = fExpr
    where
      fExpr (Var a) = f1 a
      fExpr (LitI a) = f2 a
      fExpr (LitB a) = f3 a
      fExpr LitNull = f4
      fExpr (Parens a) = f5 (fExpr a)
      fExpr (ArrayElem a b) = f6 (fExpr a) (fExpr b)
      fExpr (OpNeg a) = f7 (fExpr a)
      fExpr (BinopExpr a b c) = f8 a (fExpr b) (fExpr c)
      fExpr (Forall a b) = f9 a (fExpr b)
      fExpr (SizeOf a) = f10 (fExpr a)
      fExpr (RepBy a b c) = f11 (fExpr a) (fExpr b) (fExpr c)
      fExpr (Cond a b c) = f12 (fExpr a) (fExpr b) (fExpr c)
      fExpr (NewStore a) = f13 (fExpr a)
      fExpr (Dereference a) = f14 a
