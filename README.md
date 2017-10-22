# MeanDiff

If you want to see the details about the system, go to
[here](https://softsec-kaist.github.io/MeanDiff/).

## Table of Contents

* [Installation](#installation)
* [Usage](#usage)
* [Testing additional lifters](#testing-additional-lifters)
  * [UIR](#uir)
    * [Abstract Syntax](#abstract-syntax)
    * [Semantics](#semantics)
  * [Writing translator for UIR](#writing-translator-to-uir)
  * [Adding translator to MeanDiff](#adding-translator-to-meandiff)
* [Contributing](#contributing)
* [Sponsors](#sponsors)

## Installation

T.B.D.

## Usage

T.B.D.

## Testing additional lifters

### UIR

UIR stands for `Unified Intermediate Representation`. UIR is used for unifying
every BBIR into a single form. UIR is a simple, but Turing-complete language. It
is also designed to be explicit and self-contained.

#### Abstract Syntax

```
<EndianT> ::= BE | LE

<UnOpT>   ::= NEG | NOT

<BinOpT>  ::= ADD | SUB | UMUL | SMUL | UDIV | SDIV | UMOD | SMOD | SHL | USHR
            | SSHR | AND | OR | XOR | CONCAT

<RelOpT>  ::= EQ | NEQ | ULT | SLT | ULE | SLE

<CastOpT> ::= LOW | HIGH | ZERO | SIGN

<Expr>    ::= <Num> | <Var> | [<Expr>]:<Size> | <UnOpT> <Expr>
            | <Expr> <BinOpT> <Expr> | <Expr> <RelOpT> <Expr>
            | <Expr> -> <CastOpT>:<Size> | If <Expr> <Expr> <Expr> | Undefined

<Stmt>    ::= Start <Value> <Size> <EndianT> | <Reg> := <Expr>
            | [<Expr>] := <Expr> | <Symbol> | If <Expr> <Symbol> <Symbol>
            | End <Expr> | Unrecognized

<Stmts>   ::= <Stmt> :: <Stmts> | []

<AST>     ::= <Stmts> | <Uninterpretable> | <Incapable>
```

#### Semantics

T.B.D.

### Writing translator for UIR

T.B.D.

### Adding translator to MeanDiff

T.B.D.

## Contributing

T.B.D.

## Acknowledgement

The work was supported by Institute for Information & communications Technology Promotion (IITP) grant funded by the Korea government (MSIT).
