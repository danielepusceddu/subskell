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
