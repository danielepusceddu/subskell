open Help

type ide = string

type typing = 
  | TInt
  | TBool
  | TFun of typing * typing

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

type env = ide -> expr option and

term = 
  | CNum of int
  | CBool of bool
  | EFun of ide * expr

  (* runtime only *)
  | EClosure of ide * expr * env (* shouldn't need env list *)

and nonterm =
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EBinOp of expr * binop * expr
  | EUnOp of unop * expr
  | EVar of ide
  | ELetIn of ide * expr * expr

  (* runtime only *)
  | ERet of expr
  | EThunk of expr * env 
  
and expr = 
  | ET of term
  | ENT of nonterm(* for lazy evaluation *)

type typesig = ide * typing

type declaration = ide * expr

type typesig_or_decl =
  | STypeSig of typesig
  | SDecl of declaration

type program = typesig list * declaration list * expr

type typenv = ide -> typing option
type envstack = env list
let tbottom _ = None
let bottom _ = None

let explode_typesig_or_decl_list (tdl: typesig_or_decl list) : (typesig list * declaration list) =
  let rec helper tdl tl dl = 
    match tdl with
      | [] -> ([], [])
      | hd::l -> (let (tailt, taild) = helper l tl dl in match hd with
        | STypeSig t -> (t::tailt, taild)
        | SDecl d -> (tailt, d::taild)
      )
  in helper tdl [] []
;;

let assert_no_doubles (dl: declaration list) (tl: typesig list) = 
  let dl_names = List.fold_left (fun acc (x,_) -> x::acc) [] dl in
  let tl_names = List.fold_left (fun acc (x,_) -> x::acc) [] tl in
  if not (has_dups dl_names) && not (has_dups tl_names)
    then () else failwith "Double declaration or type signature"
;;