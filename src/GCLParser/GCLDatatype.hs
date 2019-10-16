module GCLParser.GCLDatatype where
    
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
              , stmt   :: Stmt
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
    | DrefAssign String           Expr
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
    show (DrefAssign var e)       = var ++ ".val := " ++ show e 
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
    | ArrayElem          Expr   Expr   
    | OpNeg              Expr    
    | BinopExpr          BinOp  Expr   Expr
    | Forall             String Expr 
    | Exists             String Expr 
    | SizeOf             Expr
    | RepBy              Expr   Expr   Expr
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
    
instance Show Expr where
    show (Var var)                  = var
    show (LitI x)                   = show x
    show (LitB True)                = "true"
    show (LitB False)               = "false"
    show LitNull                    = "null"
    show (Dereference u)            = u ++ ".val"
    show (Parens e)                 = "(" ++ show e ++ ")"
    show (ArrayElem var index)      = show var ++ "[" ++ show index ++ "]"
    show (OpNeg expr)               = "~" ++ show expr
    show (BinopExpr op e1 e2)       = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (NewStore e)               = "new(" ++ show e ++ ")"    
    show (Forall var p)             = "forall " ++ var ++ ":: " ++ show p
    show (Exists var p)             = "exists " ++ var ++ ":: " ++ show p
    show (SizeOf var)               = "#" ++ show var
    show (RepBy var i val)          = show var ++ "(" ++ show i ++ " repby " ++ show val ++ ")"
    show (Cond g e1 e2)             = "(" ++ show g ++ " -> " ++ show e1 ++ " | " ++ show e2 ++ ")"
    
instance Show BinOp where
    show And = "&&"
    show Or = "||" 
    show Implication = "==>" 
    show LessThan = "<"
    show LessThanEqual = "<=" 
    show GreaterThan = ">"
    show GreaterThanEqual = ">="
    show Equal = "="
    show Minus = "-" 
    show Plus = "+"
    show Multiply = "*"
    show Divide = "/"
    show Alias = "=="  
