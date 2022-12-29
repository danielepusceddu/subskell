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