type ide = string

type typing = 
  | TInt
  | TBool
  | TFun of typing * typing
  | TVar of int

type tscheme = int list * typing

type binop =
  | BOPlus
  | BOTimes
  | BOMinus
  | BOAnd
  | BOOr
  | BOEq
  | BOLeq

type unop =
  | UONot

type name =
  | NVar of ide
  | NBop of binop
  | NUop of unop

type pexpr = 
  | PNum of int
  | PBool of bool
  | PFun of ide * pexpr
  | PApp of pexpr * pexpr
  | PIf of pexpr * pexpr * pexpr
  | PName of name
  | PLetIn of ide * (tscheme option) * pexpr * pexpr