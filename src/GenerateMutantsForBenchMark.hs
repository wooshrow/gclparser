--
-- Contains a function for generating mutants of all
-- the programs in the PV benchmark-set.
--
module GenerateMutantsForBenchMark
where

import GCLParser.Parser
import GCLParser.PrettyPrint
import MuGCL
import System.Directory

originalProgFolder = "examples/benchmark/"
outputFolder = "examples/benchmark/mutants/"

targets = ["min","memberOf", "pullUp", "divByN", "find12", "bsort"]

--
-- Will generate the mutants of a single program. The program is
-- assumed to be located in originalProgFolder.
--
generateMutants programName = do
   let programFile = originalProgFolder ++ programName ++ ".gcl"
   parsed <- parseGCLfile programFile
   let Right program = parsed
   let prgSrc = ppProgram2String program
   putStrLn ("** Mutating " ++ programName)
   let mutants = mutateProgram program
   let mutantNames = [ programName ++ "_M" ++ show k ++ "_" ++ show mty ++ ".gcl"| (k,(mty,_)) <- zip [0..] mutants ]
   putStrLn ("   producing " ++ show (length mutantNames) ++ " mutants.")
   let outputFolder__ = outputFolder ++ "/" ++ programName ++ "/"
   createDirectoryIfMissing False outputFolder__
   --print mutantNames
   sequence_ [ writeFile (outputFolder__ ++ fname) (ppProgram2String mutSrc) | (fname,(_,mutSrc)) <- zip mutantNames mutants ]
   --writeFile outputfile prgSrc
   putStrLn "   DONE."

--
-- will generate the mutants of all programs in the benchmark-set.
--
generateAllMutants = do
    sequence_ [ generateMutants target | target <- targets ]
