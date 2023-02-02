open Ast

let string_of_binop = function
  | BOPlus -> "+"
  | BOMinus -> "-"
  | BOTimes -> "*"
  | BOAnd -> "&&"
  | BOOr -> "||"
  | BOEq -> "?="
  | BOLeq -> "<="

let string_of_unop = function
  | UONot -> "not"
