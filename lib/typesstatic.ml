open Typescommon
open Base26p

(* The types in our language*)
type typing = 
  | TInt
  | TBool
  | TFun of typing * typing
  | TVar of int

(* Type Schemes in our language*)
type tscheme = int list * typing

(* Parsing AST *)
type pexpr = 
  | PNum of int
  | PBool of bool
  | PFun of ide * pexpr
  | PApp of pexpr * pexpr
  | PIf of pexpr * pexpr * pexpr
  | PName of name
  | PLetIn of ide * (tscheme option) * pexpr * pexpr

(* AST in which type annotations are forced *)
type aexpr = 
  | ANum of int
  | ABool of bool
  | AFun of ide * aexpr
  | AApp of aexpr * aexpr
  | AIf of aexpr * aexpr * aexpr
  | AName of name
  | ALetIn of ide * tscheme * aexpr * aexpr

(* Types for type environment *)
module NameMap = Map.Make(struct type t = name let compare = compare end);;
type tenv = tscheme NameMap.t;;

(* Types for type constraints *)
type constr = typing * typing;;
module ConstrSet = Set.Make(struct type t = constr let compare = compare end);;

(* Type for type substitutions *)
type tsubst = int * typing

(* Type for type inference errors *)
type infer_error = 
  | NameWithoutType of name 
  | UnsatConstr of pexpr * (constr list)
  | BadTypeHint of ide * tscheme * tscheme


(* Pretty printing of these types *)
let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun(TFun(t11, t12), t2) -> "(" ^ (string_of_type (TFun(t11, t12))) ^ ") -> " ^ (string_of_type t2)
  | TFun(t1, t2) -> (string_of_type t1) ^ " -> " ^ (string_of_type t2)
  | TVar n -> "\'" ^ (base26p_of_int n)
;;

let string_of_tscheme = function
  | ([], t) -> string_of_type t
  | (l, t) -> 
    let l' = List.map (fun i -> "'" ^ (base26p_of_int i)) l in
    let quant_str = (String.concat " " l') in
    quant_str ^ ". " ^ (string_of_type t)
;;

let string_of_constr ((t1,t2): constr) = 
  "(" ^ (string_of_type t1) ^ ") = (" ^ (string_of_type t2) ^ ")"

let string_of_constrl (cl: constr list) =
  let l' = List.map string_of_constr cl in
  String.concat " and " l'

let string_of_infer_error err = 
  "TypeInfer error: " ^ match err with
  | NameWithoutType(n) -> "name \"" ^ (string_of_name n) ^ "\" without type"
  | UnsatConstr(_, constrs) -> "Unsatisfiable constraints " ^ (string_of_constrl constrs)
  | BadTypeHint(x, hint, inferred) -> "bad type hint for " ^ x ^ ": (" ^ (string_of_tscheme hint) ^ "), should be " ^ (string_of_tscheme inferred)