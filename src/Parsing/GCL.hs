module Parsing.GCL where
    
-- import Data.List

type Verbose = Bool

type Depth = Int

-----------------------------------------------------------------------------
-- Program
-----------------------------------------------------------------------------

data PrimitiveType 
    = PTInt 
    | PTBool
    deriving (Show, Eq)

data Type 
    = PType PrimitiveType  -- primitive tyoe
    | RefType
    | AType PrimitiveType  -- array type, one dimensional
    deriving (Show, Eq)

data VarDeclaration 
    = VarDeclaration String Type
    deriving (Show)

{-
data Procedure 
    = Procedure String [VarDeclaration] [VarDeclaration] Expr Expr
    deriving (Show)
-}

data Program 
    = Program { 
--                pre    :: Expr, 
              name   :: String 
              , input  :: [VarDeclaration]
              , output :: [VarDeclaration]
              , stat   :: Stmt
--              , procs  :: [Procedure]
--              , post   :: Expr 
              } 
    deriving (Show)

data Stmt
    = Skip       
    | Assert     Expr             
    | Assume     Expr             
    | Assign     String           Expr   
    | AAssign    String           Expr   Expr  
    | Seq        Stmt             Stmt   
    | IfThenElse Expr             Stmt   Stmt     
    | While      Expr             Stmt   
    | Block      [VarDeclaration] Stmt   
    | TryCatch   String           Stmt   Stmt
--    | Call       [String]         [Expr] String

instance Show Stmt where
    show Skip                     = "skip"
    show (Assert condition)       = "assert " ++ show condition
    show (Assume condition)       = "assume " ++ show condition
    show (Assign var e)           = var ++ " := " ++ show e 
    show (AAssign var i e)        = var ++ "[" ++ show i ++ "]" ++ " := " ++ show e
    show (Seq s1 s2)              = show s1 ++ ";" ++ show s2 
    show (IfThenElse gaurd s1 s2) = "if " ++ show gaurd ++ " then " ++ show s1 ++ " else " ++ show s2
    show (While gaurd s)          = "while " ++ show gaurd ++ " do {" ++ show s ++ "}"
    show (Block vars s)           = "var " ++ show vars ++ " {" ++ show s ++ "}"
    show (TryCatch e s1 s2)         = "try { " ++ show s1 ++ " } catch(" ++ e ++ "){ " ++ show s2 ++ " }"
--    show (Call vars args f)       = "(" ++ intercalate "," vars ++ ") := " ++ "(" ++ (intercalate "," . map show) args ++ ")"
    
-----------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------
    
data Expr 
    = Var                String  
    | LitI               Int     
    | LitB               Bool    
    | LitNull
    | Parens             Expr    
    | ArrayElem          String Expr   
    | OpNeg              Expr    
    | BinopExpr          BinOp  Expr   Expr
    | Forall             String Expr 
    | SizeOf             String  
    | RepBy              String Expr   Expr
    | Cond               Expr   Expr   Expr
    | NewStore           Expr
    | Dereference        String
    deriving (Eq) 
    
data BinOp = And | Or | Implication 
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Equal
    | Minus | Plus | Multiply | Divide
    | Alias
    deriving (Eq)

opAnd :: Expr -> Expr -> Expr
opAnd = BinopExpr And
opOr :: Expr -> Expr -> Expr
opOr  = BinopExpr Or
opImplication :: Expr -> Expr -> Expr
opImplication = BinopExpr Implication
opLessThan :: Expr -> Expr -> Expr
opLessThan = BinopExpr LessThan
opLessThanEqual :: Expr -> Expr -> Expr
opLessThanEqual = BinopExpr LessThanEqual
opGreaterThan :: Expr -> Expr -> Expr
opGreaterThan   = BinopExpr GreaterThan
opGreaterThanEqual :: Expr -> Expr -> Expr
opGreaterThanEqual = BinopExpr GreaterThanEqual
opEqual :: Expr -> Expr -> Expr
opEqual = BinopExpr Equal
opMinus :: Expr -> Expr -> Expr
opMinus = BinopExpr Minus
opPlus :: Expr -> Expr -> Expr
opPlus = BinopExpr Plus
opMultiply :: Expr -> Expr -> Expr
opMultiply = BinopExpr Multiply
opDivide :: Expr -> Expr -> Expr
opDivide = BinopExpr Divide
opAlias :: Expr -> Expr -> Expr
opAlias    = BinopExpr Alias
exists_ :: String -> Expr -> Expr
exists_ i p = OpNeg (Forall i (OpNeg p))
    
instance Show Expr where
    show (Var var)                  = var
    show (LitI x)                   = show x
    show (LitB True)                = "true"
    show (LitB False)               = "false"
    show LitNull                    = "null"
    show (Dereference u)            = u ++ ".val"
    show (Parens e)                 = "(" ++ show e ++ ")"
    show (ArrayElem var index)      = var ++ "[" ++ show index ++ "]"
    show (OpNeg expr)               = "~" ++ show expr
    show (BinopExpr And e1 e2)              = show e1 ++ " && " ++ show e2
    show (BinopExpr Or e1 e2)               = show e1 ++ " || " ++ show e2
    show (BinopExpr Implication e1 e2)      = show e1 ++ " ==> " ++ show e2
    show (BinopExpr LessThan e1 e2)         = show e1 ++ " < " ++ show e2
    show (BinopExpr LessThanEqual e1 e2)    = show e1 ++ " <= " ++ show e2
    show (BinopExpr GreaterThan e1 e2)      = show e1 ++ " > " ++ show e2
    show (BinopExpr GreaterThanEqual e1 e2) = show e1 ++ " >= " ++ show e2
    show (BinopExpr Equal e1 e2)            = show e1 ++ " = " ++ show e2
    show (BinopExpr Minus e1 e2)            = show e1 ++ " - " ++ show e2
    show (BinopExpr Plus e1 e2)             = show e1 ++ " + " ++ show e2
    show (BinopExpr Divide e1 e2)           = show e1 ++ " / " ++ show e2
    show (BinopExpr Multiply e1 e2)         = show e1 ++ " * " ++ show e2
    show (BinopExpr Alias e1 e2)            = show e1 ++ " == " ++ show e2
    show (NewStore e)               = "new " ++ show e
    show (Forall var p)             = "forall " ++ var ++ ":: " ++ show p
    show (SizeOf var)               = "#" ++ var
    show (RepBy var i val)          = var ++ "(" ++ show i ++ " repby " ++ show val ++ ")"
    show (Cond g e1 e2)             = "(" ++ show g ++ "->" ++ show e1 ++ "|" ++ show e2 ++ ")"