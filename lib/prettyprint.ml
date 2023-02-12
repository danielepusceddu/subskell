open Parsingast
open Base26p

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

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun(TFun(t11, t12), t2) -> "(" ^ (string_of_type (TFun(t11, t12))) ^ ") -> " ^ (string_of_type t2)
  | TFun(t1, t2) -> (string_of_type t1) ^ " -> " ^ (string_of_type t2)
  | TVar n -> "\'" ^ (base26p_of_int n)