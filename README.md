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

T.B.D.

### Writing translator for UIR

T.B.D.

### Adding translator to MeanDiff

T.B.D.

## Contributing

T.B.D.

## License

This project is licensed under the [MIT License](LICENSE.md).

## Acknowledgement

The work was supported by Institute for Information & communications Technology Promotion (IITP) grant funded by the Korea government (MSIT).
