(* Identifiers *)
type ide = string

(* Binary operators in our language *)
type binop =
  | BOPlus
  | BOTimes
  | BOMinus
  | BOAnd
  | BOOr
  | BOEq
  | BOLeq

(* Unary operators in our language *)
type unop =
  | UONot

(* Names in our language
   (we treat operators as names referring to
   preexisting functions in the environment) *)
type name =
  | NVar of ide
  | NBop of binop
  | NUop of unop



(* Pretty printing of these types *)
let string_of_binop = function
  | BOPlus -> "+"
  | BOMinus -> "-"
  | BOTimes -> "*"
  | BOAnd -> "&&"
  | BOOr -> "||"
  | BOEq -> "="
  | BOLeq -> "<="

let string_of_unop = function
  | UONot -> "not"

let string_of_name = function
  | NBop(op) -> string_of_binop op
  | NUop(op) -> string_of_unop op
  | NVar x -> x