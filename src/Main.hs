module Main where

import Lexing.Lexer
import Parsing.Parser
import Parsing.GCL
-- import PrettyPrint

-- Note: "ParseResult a" was defined as an abbreviation of "Either String a"

parseGCLstring :: String -> ParseResult Program
parseGCLstring str = parseGCL . lexer $ str
    
parseGCLfile :: FilePath -> IO (ParseResult Program)
parseGCLfile path = do
    file <- readFile path
    return . parseGCL . lexer $ file


main :: IO ()
main = do
    putStr "hello!"