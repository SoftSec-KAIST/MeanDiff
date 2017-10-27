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
* [Citing MeanDiff](#citing-meandiff)
* [License](#license)
* [Acknowledgement](#acknowledgement)

## Installation

As MeanDiff has several external dependencies due to lifters and external
libraries, the whole build system is containerized with docker.

All docker images (for each lifter, etc.) are based to the `BaseImage` (T.B.D
Link), which used `ubuntu:16.10`.

### Building

Start by building submodules. This could take some time as the docker images
needs to be downloaded and built.

    make init
    make lifters
    make external

Now, MeanDiff can be built either
 * A. Inside the docker container
 * B. In your native environment

For option B., you can find dependencies in the respective `Dockerfile`s.

The resulting binaries are found in the `build` directory.

#### A.
To build and setup docker environment:

    ./build_image.sh
    ./build_src.sh

#### B.
If you want to run MeanDiff outside docker, just type `make`.

## Usage

T.B.D.

To log into the docker container for MeanDiff, taged `build_meandiff`:

    docker run -v $(pwd):/src -ti build_meandiff:latest

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

Here, only non-trivial semantics of UIR are shown.

Primitive types: `<Num>`, `<Var>`, `<Size>`, `<Symbol>`

`<Expr>`:
 - `[<Expr>]:<Size>`: Load a value of size `<Size>` from `<Expr>`.
 - `<Expr> -> <CastOpT>:<Size>`: Enlarge or shorten `<Expr>` of size `<Size>` by
    referencing `<CastOpT>`.
 - `If <Expr0> <Expr1> <Expr2>`: If-Then-Else expression. `<Expr1>` and
    `<Expr2>` should have same type.

`<Stmt>`:
 - `Start <Value> <Size> <EndianT>`: Indicate information aboutn an instruction
    and system. The target instruction is placed at `<Value>` with length
    `<Size>`, and has the endianness of `<EndianT>`.
 - `[<Expr1>] := <Expr2>`: Store `<Expr2>` into the memory pointed by `<Expr1>`.
 - `End <Expr>`: Indicates the end of control-flow of IR statements. `<Expr>`
    represents the address of next instruction.

### Writing translator for UIR

There are some rules for a translator from your IR to our UIR.
 - Register names must be in lower cases
 - For each expression and statement, we have a simple type system. See
    `src/MeanDiff/Type.fs`
 - The first item in `<Stmts>` should be `Start`
 - `End` must be placed at the end of the instruction semantics.

### Adding translator to MeanDiff

In order to add your translator to MeanDiff, you need to manually modify
`src/MeanDiff/Report.fs` appropriately.

## Citing MeanDiff

To cite our paper:

```
@INPROCEEDINGS{kim:ase2017,
    author = {Soomin Kim, Markus Faerevaag, Minkyu Jung, SeungIl Jung, DongYeop Oh, JongHyup Lee, and Sang Kil Cha},
    title = {Testing Intermediate Representations for Binary Analysis},
    booktitle = {Proceedings of the 32nd IEEE/ACM International Conference on Automated Software Engineering},
    year = {2017},
    pages = {353--364}
}
```

## License

This project is licensed under the [MIT License](LICENSE.md).

## Acknowledgement

The work was supported by Institute for Information & communications Technology Promotion (IITP) grant funded by the Korea government (MSIT).
