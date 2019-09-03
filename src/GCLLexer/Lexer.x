{
{-# OPTIONS_GHC -w #-}
module GCLLexer.Lexer(
      lexer
    , Token(..)
) where

import GCLLexer.Token
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    $white+			            	;
    "skip"                          {\s -> TSkip }
    "assert"                        {\s -> TAssert }
    "assume"                        {\s -> TAssume }
    "var"                           {\s -> TVar }
    "in"                            {\s -> TIn }
    "while"                         {\s -> TWhile }
    "do"                            {\s -> TDo }
    "if"                            {\s -> TIf }
    "then"                          {\s -> TThen }
    "else"                          {\s -> TElse }
    "end"                           {\s -> TEnd }
    "try"                           {\s -> TTry }
    "catch"                         {\s -> TCatch }
    ":="                            {\s -> TAssign }
    ";"                             {\s -> TSemicolon }
    ":"                             {\s -> TColon }
    "["                             {\s -> TSOpen }
    "]"                             {\s -> TSClose }
    "{"                             {\s -> TCOpen }
    "}"                             {\s -> TCClose }
    "("                             {\s -> TPOpen }
    ")"                             {\s -> TPClose }
    "~"                             {\s -> TNeg }
    "+"                             {\s -> TPlus }
    "-"                             {\s -> TMinus }
    "/"                             {\s -> TDivide }
    "*"                             {\s -> TMultiply }
    "&&"                            {\s -> TAnd }
    "#"                             {\s -> TSizeOf }
    "||"                            {\s -> TOr }
    "==>"                           {\s -> TImplication }
    "="                             {\s -> TEqual }
    "=="                            {\s -> TAlias }    
    "<"                             {\s -> TLessThan }
    "<="                            {\s -> TLessThanEqual }
    ">"                             {\s -> TGreaterThan }
    ">="                            {\s -> TGreaterThanEqual }
    ","                             {\s -> TComma }
    "."                             {\s -> TDot }
    "forall"                        {\s -> TForall }
    "exists"                        {\s -> TExists }
    "|"                             {\s -> TBar }
    "null"                          {\s -> TNull }
    "val"                           {\s -> TVal } 
    "new"                           {\s -> TNew }
    "ref"                           {\s -> TRef }
    "int"                           {\s -> TInt }
    "bool"                          {\s -> TBool }
    "true"                          {\s -> TBoolValue True }
    "false"                         {\s -> TBoolValue False }
    $digit+                         {\s -> TIntValue (read s) }
    $alpha [$alpha $digit \_ \']*   {\s -> TIdent s }

{
lexer :: String -> [Token]
lexer = alexScanTokens
}