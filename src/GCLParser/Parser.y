{
{-# OPTIONS_GHC -w #-}
module GCLParser.Parser where

import GCLLexer.Lexer
import GCLLexer.Token
import GCLParser.GCLDatatype

import Debug.Trace
}

%name parseGCL
%tokentype { Token }
%error     { parseError }
%monad     { ParseResult } { (>>=) } { return }

%token 
    identifier       { TIdent     $$     }
    intvalue         { TIntValue  $$     }
    boolvalue        { TBoolValue $$     }
    skip             { TSkip             }
    assert           { TAssert           }
    assume           { TAssume           }
    var              { TVar              }
    in               { TIn               }
    while            { TWhile            }
    do               { TDo               }
    if               { TIf               }
    then             { TThen             }
    else             { TElse             }
    end              { TEnd              }
    try              { TTry              }
    catch            { TCatch            }
    assign           { TAssign           }
    semicolon        { TSemicolon        }
    colon            { TColon            }
    sopen            { TSOpen            }
    sclose           { TSClose           }
    copen            { TCOpen            }
    cclose           { TCClose           }
    popen            { TPOpen            }
    pclose           { TPClose           }
    neg              { TNeg              } 
    plus             { TPlus             }
    minus            { TMinus            }
    divide           { TDivide           }
    multiply         { TMultiply         }
    and              { TAnd              }
    or               { TOr               }
    implication      { TImplication      }
    equal            { TEqual            }
    lessthan         { TLessThan         }
    lessthanequal    { TLessThanEqual    }
    greaterthan      { TGreaterThan      }
    greaterthanequal { TGreaterThanEqual }
    comma            { TComma            }
    dot              { TDot              }
    bar              { TBar              }
    forall           { TForall           }
    exists           { TExists           }
    sizeof           { TSizeOf           }
    int              { TInt              }
    bool             { TBool             }
    ref              { TRef              }
    new              { TNew              }
    null             { TNull             }
    val              { TVal              }
    alias            { TAlias            }


%left implication
%left or and 
%left neg

%nonassoc alias equal
%nonassoc lessthan lessthanequal greaterthan greaterthanequal 

%left plus minus
%left multiply divide

%%

-- | Program parsing

PProgram    :: { Program }
--             : PConditions identifier popen PVarDeclarations bar PVarDeclarations pclose copen PStatements cclose PConditions PProcedures
--                { Program $1 $2 $4 $6 $9 $12 $11 }
             : identifier popen PVarDeclarations bar PVarDeclarations pclose copen PStatements cclose 
                { Program $1 $3 $5 $8 }

--PProcedures :: { [Procedure] }
--             : PProcedure PProcedures { $1 : $2 }
--             | PProcedure             { [$1] }
--             |                        { [] }

--PProcedure :: { Procedure }
--              : PConditions identifier popen PVarDeclarations bar PVarDeclarations pclose PConditions
--                 { Procedure $2 $4 $6 $1 $8 }

--PConditions :: { Expr }
--             : copen PExpr cclose { $2 }

PVarDeclarations :: { [VarDeclaration] }
                  : PVarDeclaration comma PVarDeclarations { $1 : $3 }
                  | PVarDeclaration                        { [$1] }
                  |                                        { [] }

PVarDeclaration :: { VarDeclaration }
                 : identifier colon PType { VarDeclaration $1 $3 }
                 
PType :: { Type }
       : sopen sclose PPrimitiveType { AType $3 }
       | PPrimitiveType              { PType $1 }  
       | ref                         { RefType }

PPrimitiveType :: { PrimitiveType }
                : int  { PTInt }
                | bool { PTBool }

-- | Statement parsing

PStatements :: { Stmt }
             : PStatements semicolon PStatements { Seq $1 $3 }
             | PStatement                        { $1 }

--PArguments :: { [Expr] }
--            : PExpr comma PArguments { $1 : $3 }
--            | PExpr                  { [$1] }
--            |                        { [] }

PStatement  :: { Stmt }
             : skip                                                   { Skip }
             | assert PExpr                                           { Assert $2 }
             | assume PExpr                                           { Assume $2 }
             | if PExpr then copen PStatements cclose else copen PStatements cclose { IfThenElse $2 $5 $9 }
             | while PExpr do copen PStatements cclose                { While $2 $5 }
             | var PVarDeclarations copen PStatements cclose          { Block $2 $4 }
             | try copen PStatement cclose catch popen identifier pclose copen PStatement cclose { TryCatch $7 $3 $10 }
             | identifier sopen PExpr sclose assign PExpr             { AAssign $1 $3 $6 }
             | identifier assign PExpr                                { Assign $1 $3 }
             | identifier dot val assign PExpr                        { DrefAssign $1 $5 }
--           | popen PIdentifiers pclose assign identifier popen PArguments pclose { Call $2 $7 $5 }

PIdentifiers :: { [String] }
              : identifier comma PIdentifiers { $1 : $3 }
              | identifier                    { [$1] }
              |                               { [] }

-- | Expression parsing

PExpr :: { Expr }
       : identifier                                     { Var $1  }
       | intvalue                                       { LitI $1 }
       | minus intvalue                                 { LitI (-$2) }
       | boolvalue                                      { LitB $1 }
       | popen PExpr pclose                             { Parens $2 }
       | identifier sopen PExpr sclose                  { ArrayElem (Var $1) $3 }
       | sizeof identifier                              { SizeOf (Var $2) }
       | neg PExpr                                      { OpNeg $2 } 
       | PExpr and PExpr                                { opAnd $1 $3 }
       | PExpr or PExpr                                 { opOr $1 $3 }
       | PExpr implication PExpr                        { opImplication $1 $3 }
       | PExpr lessthan PExpr                           { opLessThan $1 $3 }
       | PExpr lessthanequal PExpr                      { opLessThanEqual $1 $3 }
       | PExpr greaterthan PExpr                        { opGreaterThan $1 $3 }
       | PExpr greaterthanequal PExpr                   { opGreaterThanEqual $1 $3 }
       | PExpr equal PExpr                              { opEqual $1 $3 }
       | PExpr minus PExpr                              { opMinus $1 $3 }
       | PExpr plus PExpr                               { opPlus $1 $3 }
       | PExpr divide PExpr                             { opDivide $1 $3 }
       | PExpr multiply PExpr                           { opMultiply $1 $3 }
       | PExpr alias PExpr                              { opAlias $1 $3 }
       | new popen PExpr pclose                         { NewStore $3 }
       | null                                           { LitNull }
       | identifier dot val                             { Dereference $1 }
       
       
-- I suppress the introduction of fresh name here. You should decide it yourself.       
--       | forall identifier colon colon PExpr            { let newName = $2 ++ "'"
--                                                              in Forall newName (rename newName $2 $5) }

       | forall identifier colon colon PExpr            { Forall $2 $5 }
       | exists identifier colon colon PExpr            { Exists $2 $5 }

{
type ParseResult a = Either String a

rename :: String -> String -> Expr -> Expr
rename new old (Parens e)                 = Parens (rename new old e)
rename new old (OpNeg e)                  = OpNeg (rename new old e)
rename new old (BinopExpr opr e1 e2)      = BinopExpr opr (rename new old e1) (rename new old e2)
rename new old (Forall x e)               = Forall x (rename new old e)
rename new old (ArrayElem var e)          = ArrayElem var (rename new old e)
rename _   _   (LitI x)                   = LitI x
rename _   _   (LitB x)                   = LitB x
rename new old (SizeOf x)                 = SizeOf x
rename new old (Var x)
    | x == old  = Var new
    | otherwise = Var x

parseError :: [Token] -> ParseResult a
parseError tokens = Left $ "Failed to parse from: " ++ show tokens

-- Pre-processor to strip out comment; currently only single-line comments are supported.
-- A comment-line starts with "//"
stripComment :: String -> String
stripComment input = unlines . map stripCommentWorker . lines $ input
   where
   stripCommentWorker ('/':'/':s) = eatComment s
      where
      eatComment z@('\n' : s) = z
      eatComment (x : s) = eatComment s
      eatComment [] = []
   stripCommentWorker (x:s) = x : stripCommentWorker s
   stripCommentWorker [] = []

      

-- Parse a string containing a GCL program
parseGCLstring :: String -> ParseResult Program
parseGCLstring str = parseGCL . lexer . stripComment $ str
    
-- Parse a GCL program from a text file    
parseGCLfile :: FilePath -> IO (ParseResult Program)
parseGCLfile path = do
    file <- readFile path
    return . parseGCLstring $ file
}
