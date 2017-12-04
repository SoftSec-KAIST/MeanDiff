module MeanDiff.UIR

//////////////////////////////////////////
// Unified Intermediate Representation  //
//////////////////////////////////////////


//////////////////
// Basic Types  //
//////////////////

type Value = uint64
type Size = uint16    // 16bits are enough to represent a size
type Symbol = string  // Symbol is used for both register names and labels


//////////////////////
// Endian Constants //
//////////////////////

type EndianT =
  | BE
  | LE


////////////////////////
// Operator Constants //
////////////////////////

type UnOpT =    // Unary Operator
  | NEG
  | NOT

type BinOpT =   // Binary Operator
  | ADD
  | SUB
  | UMUL
  | SMUL
  | UDIV
  | SDIV
  | UMOD
  | SMOD
  | SHL
  | USHR
  | SSHR
  | AND
  | OR
  | XOR
  | CONCAT

type RelOpT =   // Relational Operator
  | EQ
  | NEQ
  | ULT
  | SLT
  | ULE
  | SLE

type CastOpT =  // Casting Operator
  | LOW
  | HIGH
  | ZERO
  | SIGN


////////////////////
// Variable Type  //
////////////////////

type Reg = Symbol * Size


////////////////
// Expression //
////////////////

type Expr =
  | Num of Value * Size               // value, size
  | Var of Reg                        // reg
  | Load of Expr * Size               // addr, size
  | UnOp of UnOpT * Expr              // op, expr
  | BinOp of BinOpT * Expr * Expr     // op, expr1, expr2
  | RelOp of RelOpT * Expr * Expr     // op, expr1, expr2
  | Cast of CastOpT * Size * Expr     // op, size, expr
  | Ite of Expr * Expr * Expr         // cond, thenExpr, elseExpr
  | Undefined // Undefined Value case defined in Manual


////////////////
// Statement  //
////////////////

type Stmt =
  | Start of Value * Size * EndianT   // addr, len, endian
  | Move of Reg * Expr                // reg, expr
  | Store of Expr * Expr              // addr, expr
  | Label of Symbol                   // lbl
  | CJump of Expr * Symbol * Symbol   // cond, thenLbl, elseLbl
  | End of Expr                       // addr
  | Unrecognized // Statement that is not currently supported by UIR


//////////////////////////
// Abstract Syntax Tree //
//////////////////////////

type AST =
  | Stmts of Stmt list                // When lifting is success
  | Uninterpretable                   // When lifting is failed because of
                                      // lifter
  | Incapable                         // When lifting is failed because of
                                      // translator
let getStmts = function
  | Stmts (sl) -> sl
  | _ -> []
