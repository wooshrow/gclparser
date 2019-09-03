module GCLLexer.Token where

data Token
    = TIdent     String
    | TIntValue  Int
    | TBoolValue Bool
--    | TArray     Int
    | TNull
    | TSkip
    | TAssert
    | TAssume
    | TVar
    | TIn
    | TWhile
    | TDo
    | TIf
    | TThen
    | TElse
    | TEnd
    | TTry
    | TCatch
    | TAssign
    | TSemicolon
    | TColon
    | TSOpen
    | TSClose
    | TCOpen
    | TCClose
    | TPOpen
    | TPClose
    | TNeg
    | TNew
    | TAlias
    | TPlus
    | TMinus
    | TDivide
    | TMultiply
    | TAnd
    | TOr
    | TImplication
    | TEqual
    | TSizeOf
    | TLessThan
    | TLessThanEqual
    | TGreaterThan
    | TGreaterThanEqual
    | TComma
    | TDot
    | TBar
    | TForall
    | TExists
    | TInt
    | TBool
    | TRef
    | TVal
    deriving (Show)