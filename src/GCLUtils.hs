module GCLUtils(
   parseGCLstring,
   parseGCLfile,
   ppProgram2String,
   echoTestParser
)
where
import GCLParser.GCLDatatype
import GCLParser.Parser
import GCLParser.PrettyPrint
import Data.Either

echoTestParser filename = do
   gcl <- parseGCLfile filename
   let (Right prg) = gcl
   putStrLn . ppProgram2String $ prg
