# GCL Parser

A simple parser for the GCL language used in the course _Program Verification_. The parser is written using Happy and Haskell. A Cabal build file is included to build the library.

This parser admits a richer language than the base GCL to also accommodate optional assignments. You can ignore the parts of the parser/syntax that you don't need in your parts of assignments.

#### Prerequisites
To compile the tool, the following package are required:
* array
* containers
* quickcheck
* optparse-applicative
* pretty.

#### Compilation
To compile the library, run the command `cabal build`.

#### Usage


#### Credits

Many thanks to Stefan Koppier for providing the initial implementation of the parser.

**Contributors:** Stefan Koppier, Wishnu Prasetya
