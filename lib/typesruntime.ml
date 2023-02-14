open Typescommon

type env = name -> expr option and

term = (* Terminal: these expressions would not produce further steps. *)
  | CNum of int
  | CBool of bool
  | EFun of ide * expr

  (* These are not found in ParsingAst *)
  | EClosure of ide * expr * env (* For first-class functions *)

and nonterm = (* These expressions do produce further steps. *)
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EName of name
  | ELetIn of ide * expr * expr (* Type hints are removed from the parsing AST *)

  (* These are not found in ParsingAst *)
  | EBPrim of expr * binop * expr (* Call the implementation of primitive binary operators *)
  | EUPrim of unop * expr
  | ERet of expr (* To mark when we should pop the environment stack *)
  | EThunk of expr * env (* For lazy evaluations *)
  
and expr = 
  | ET of term
  | ENT of nonterm
;;