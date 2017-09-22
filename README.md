# MeanDiff

If you want some academic details, go to
[here](https://softsec-kaist.github.io/MeanDiff/).

## Table of Content

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

### Writing translator for UIR

### Adding translator to MeanDiff

## Contributing

## Sponsors
