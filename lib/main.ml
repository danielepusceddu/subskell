open Parsingast
open Runtimeast
open Typecheck

let parse (s : string) : program =
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
  | PLetIn(x,p1,p2) -> ENT(ELetIn(x, pars2rtime p1, pars2rtime p2))
;;

type evalerr =
  | ProgCheckErr of progCheckError
  | RuntimeErr of expr * string

let eval_prog (max: int) ((tl, dl, ex): program) : (expr, evalerr) result =
  match typecheck_prog (tl,dl,ex) with
    | Error(p) -> Error(ProgCheckErr p)
    | Ok(_,_) -> (
      let (dl': (string * expr) list) = List.map (fun (x,pe) -> (x,pars2rtime pe)) dl in
      let outer_env = 
        List.fold_left (fun env (ide,expr) -> bind env (NVar ide) (Some expr)) static_env dl'
      in match eval_expr max (pars2rtime ex) [outer_env] with
        | Ok(e) -> Ok e
        | Error(e,s) -> Error(RuntimeErr(e, s))
    )
;;
  