open Ast

let string_of_binop = function
  | BOPlus -> "+"
  | BOMinus -> "-"
  | BOTimes -> "*"
  | BOAnd -> "&&"
  | BOOr -> "||"
  | BOIEq -> ".="
  | BOBEq -> "?="
  | BOLeq -> "<="

let string_of_unop = function
  | UONot -> "not"

let string_of_name = function
  | NBop(op) -> string_of_binop op
  | NUop(op) -> string_of_unop op
  | NVar x -> x