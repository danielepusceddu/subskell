type ide = string

type typing = 
  | TInt
  | TBool
  | TFun of typing * typing

type const = 
  | CNum of int
  | CBool of bool

type binop =
  | BOPlus
  | BOTimes
  | BOMinus
  | BOAnd
  | BOOr

type unop =
  | UONot

type expr =
  | EConst of const
  | EFun of ide * expr
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EBinOp of expr * binop * expr
  | EUnOp of unop * expr
  | EVar of ide

type typesig = ide * typing

type declaration = ide * expr

type typesig_or_decl =
  | STypeSig of typesig
  | SDecl of declaration

type program = typesig list * declaration list * expr