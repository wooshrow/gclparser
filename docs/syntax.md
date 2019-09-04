## GCL syntax

The syntax of the GCL as accepted by the parser provided by this library is described below. It is a slightly richer syntax than the basic GCL so that we can accommodate optional assignments. You can ignore the parts that you don't need.

Additionally, for your convenience, the internal data-type (see the file `src/Parsing/GCL.hs`) representing GCL's abstract syntax tree also has a representation for repby-expression and conditional expression.

#### Comments

Only single-line comments are supported. Comment starts with `//`; all characters after that, until the line-break are ignored.

#### Program and statement

_Program_  ::=  _Name_ `(` _Parameters_  `|`   _Parameter_ `)` `{` _Stmt_ `}`

_Parameters_ ::= zero or more _Parameter_ separated by comma

_Parameter_  ::= Name `:` Type

_Stmt_ ::=
* _BasicStmt_
* _BasicStmt_ `;` _Stmt_
* `if` _Expr_ `then` `{` _Stmt_ `} else {` _Stmt_ `}`
* `while` _Expr_ `do` `{` _Stmt_ `}`
* `var` _VarDecls_  `{` _Stmt_ `}`

   `VarDecls` ::= zero or more _VarDecl_ separated by comma.

   _VarDecl_ ::= _Name_ `:` _Type_

* `try {` _Stmt_ `} catch(` _Name_ `) {` _Stmt_ `}`

_BasicStmt_ ::=
* `skip`
* `assert` _Expr_
* `assume` _Expr_
* _Name_ `:=` _Expr_
* _Name_ `[` _Expr_ `]` `:=` _Expr_

#### Expression

_Expr_ ::=
* _Literal_

   Boolean literals are `true` and `false`. Integer literals are as usual. Then we also have `null` as the only ref-typed literal.
* _Name_ (identifier/variable-name)
* _Expr_  _BinaryOp_  _Expr_

    For the binary operator we have:

    * Arithmetic operators: +, -, *, /
    * Logical operators: &&, ||, ==>
    * Arithmetic comparators: <, <=, > , >=
    * Equality comparator: =
    * Reference/pointer equality comparator: ==

* `~` _Expr_  // negation
* _Name_ `[` _Expr_ `]`
* `( forall` _Name_  `::` _Expr_ `)`
* `( exists` _Name_  `::` _Expr_ `)`
* _Name_ `.val` // dereference operator
* `new(` _Expr_ `)` // creating a fresh store containing an int

Priority of the operators are as follows (from low to high):

```
left-associative: ==>
left-associative: || , &&
non-associative: ==, =
non-associative: <, <= , > , >=
left-associative: +, -
left-associative: *, /
```

So, e.g. `1+2*3` is parsed as `1+(2*3)`, and `x-y-z` is parsed as `(x-y)-z`.

#### Type

_Type_ ::=

* _PrimitiveType_ , which is either `int` or `bool`
* _ArrayType_
* `ref` // reference type

_ArrayType_ ::= `[]` _PrimitiveType_
