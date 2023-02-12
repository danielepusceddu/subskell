open Help

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
  | PLetIn of ide * pexpr * pexpr

type typesig = ide * typing

type declaration = ide * pexpr

type typesig_or_decl =
  | STypeSig of typesig
  | SDecl of declaration

type program = typesig list * declaration list * pexpr

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