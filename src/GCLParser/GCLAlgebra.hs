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
foldGCLProgram (p1, s1) = fProgram
    where
      fProgram (Program name ids ods stmt) = p1 name ids ods (fStmt stmt)
      fStmt = foldGCLStmt s1

foldGCLStmt :: GCLStmtAlgebra s e -> Stmt -> s
foldGCLStmt (s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, e1) = fStmt
    where
      fStmt Skip = s1
      fStmt (Assert expr) = s2 (fExpr expr)
      fStmt (Assume expr) = s3 (fExpr expr)
      fStmt (Assign name expr) = s4 name (fExpr expr)
      fStmt (AAssign name index expr) = s5 name (fExpr index) (fExpr expr)
      fStmt (DrefAssign name expr) = s6 name (fExpr expr)
      fStmt (Seq stmt1 stmt2) = s7 (fStmt stmt1) (fStmt stmt2)
      fStmt (IfThenElse expr stmt1 stmt2) = s8 (fExpr expr) (fStmt stmt1) (fStmt stmt2)
      fStmt (While expr stmt) = s9 (fExpr expr) (fStmt stmt)
      fStmt (Block ds stmt) = s10 ds (fStmt stmt)
      fStmt (TryCatch name stmt1 stmt2) = s11 name (fStmt stmt1) (fStmt stmt2)
      fExpr = foldGCLExpr e1

foldGCLExpr :: GCLExprAlgebra e -> Expr -> e
foldGCLExpr (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = fExpr
    where
      fExpr (Var name) = e1 name
      fExpr (LitI value) = e2 value
      fExpr (LitB value) = e3 value
      fExpr LitNull = e4
      fExpr (Parens expr) = e5 (fExpr expr)
      fExpr (ArrayElem expr1 expr2) = e6 (fExpr expr1) (fExpr expr2)
      fExpr (OpNeg expr) = e7 (fExpr expr)
      fExpr (BinopExpr op expr1 expr2) = e8 op (fExpr expr1) (fExpr expr2)
      fExpr (Forall name expr) = e9 name (fExpr expr)
      fExpr (SizeOf expr) = e10 (fExpr expr)
      fExpr (RepBy expr1 expr2 expr3) = e11 (fExpr expr1) (fExpr expr2) (fExpr expr3)
      fExpr (Cond expr1 expr2 expr3) = e12 (fExpr expr1) (fExpr expr2) (fExpr expr3)
      fExpr (NewStore expr) = e13 (fExpr expr)
      fExpr (Dereference name) = e14 name
