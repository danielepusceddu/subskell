open Typescommon
open Typesstatic
open Typesruntime
open Typecheck
open Runtime

let parse (s : string) : pexpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec pars2rtime (p: pexpr) : expr = match p with
  | PNum n -> ET(CNum n)
  | PBool b -> ET(CBool b)
  | PFun(x,p) -> ET(EFun(x, pars2rtime p))
  | PApp(p1,p2) -> ENT(EApp(pars2rtime p1, pars2rtime p2))
  | PIf(p1,p2,p3) -> ENT(EIf(pars2rtime p1, pars2rtime p2, pars2rtime p3))
  | PName(n) -> ENT(EName(n))
  | PLetIn(x,_,p1,p2) -> ENT(ELetIn(x, pars2rtime p1, pars2rtime p2))
;;

type evalerr =
  | ProgCheckErr of infer_error
  | RuntimeErr of expr * string

let eval_prog (max: int) (pe: pexpr) : (expr, evalerr) result =
  match tinfer_expr static_tenv pe with
    | Error(p) -> Error(ProgCheckErr p)
    | Ok(_,_) -> (match eval_expr max (pars2rtime pe) [static_env] with
        | Ok(e) -> Ok e
        | Error(e,s) -> Error(RuntimeErr(e, s))
    )
;;

let rec string_of_expr = function
  | ET(e) -> (match e with
    | CNum(n) -> string_of_int n
    | CBool(b) -> string_of_bool b
    | EFun(x, e) -> "fun " ^ x ^ " -> " ^ (string_of_expr e)
    | EClosure(x, e, _) -> "Closure(" ^ x ^ ", " ^ (string_of_expr e) ^ ", <env>)"
  )
  | ENT(e) -> (match e with
    | EApp(e1,e2) -> "(" ^ (string_of_expr e1) ^ ")("  ^ (string_of_expr e2) ^ ")"
    | EIf(e1,e2,e3) -> "if "  ^ (string_of_expr e1) ^ " then "  ^ (string_of_expr e2) ^ " else "   ^ (string_of_expr e3)
    | EName n -> string_of_name n
    | ELetIn(x,e1,e2) -> "let " ^ x ^ " = " ^ (string_of_expr e1) ^ " in "  ^ (string_of_expr e2)
    | EBPrim(e1,op,e2) -> "BPrim(" ^ (string_of_binop op) ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
    | EUPrim(op, e) -> "UPrim(" ^ (string_of_unop op) ^ ", " ^ (string_of_expr e) ^ ")"
    | ERet(e) -> "Ret(" ^ (string_of_expr e) ^ ")"
    | EThunk(e,_) -> "Thunk(" ^ (string_of_expr e) ^ ", <env>)"
  )
;;
  
let string_of_evalerr = function
  | RuntimeErr(e,s) -> "Runtime Error \"" ^ s ^ "\" at step " ^ (string_of_expr e)
  | ProgCheckErr(err) -> string_of_infer_error err
;;

let string_of_evalresult = function
  | Ok(e) -> "Result: " ^ (string_of_expr e)
  | Error(err) -> string_of_evalerr err
;;