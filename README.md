# GCL Parser

# Prerequisites
To compile the tool, the following package are required:
* array;
* containers;
* quickcheck;
* optparse-applicative;
* pretty.

# Compilation
To compile the parser, run the command `cabal build`.

# Usage
To run the tool, there are three mandatory arguments and one optional
* Input file: `-f filename`, e.g. `-f examples/E.gcl`;
* Number of test cases: `-n cases`, e.g. `-n 10`;
* Maximum program path depth: `-d depth`, e.g. `-d 20`;
* Run verbose: `-v`.

For example to test the program `E.gcl` with 10 cases for each program path of
length at most 20, run: `wlp -v -n 10 -d 20 -f examples/E.gcl`.