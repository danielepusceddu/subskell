open Ast
open Typecheck
open Prettyprint

let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bind f x e x' = if x'=x then e else f x'

let rec trace1_expr (ex: nonterm) (enstk: env list) : (expr*env list, string) result = match ex with
  | EApp(ET(EClosure(ide, expr1, saved)), expr2) ->
      Ok(ENT(ERet expr1), (bind saved (NVar ide) (Some(ENT(EThunk(expr2, List.hd enstk))))::enstk))
  | EApp(ET(EFun(ide, expr1)), expr2) ->
      Ok (ENT(ERet expr1), (bind (List.hd enstk) (NVar ide) (Some(ENT(EThunk(expr2, List.hd enstk)))))::enstk)
  | EApp(ET(_), _) -> Error("lhs of apply not a function")
  | EApp(ENT(expr1), expr2) -> (
      match trace1_expr expr1 enstk with
      | Ok(expr1', en') -> Ok(ENT(EApp(expr1', expr2)), en')
      | Error(err) -> Error(err)
  )

  | EIf(ET(CBool(true)), e, _)
  | EIf(ET(CBool(false)), _, e) -> 
      Ok(e, enstk)
  | EIf(ET(_), _, _) -> Error("trace: if without a boolean condition")

  | EIf(ENT(e1), e2, e3) -> (match trace1_expr e1 enstk with
    | Ok(e1', enstk') -> Ok(ENT(EIf(e1', e2, e3)), enstk')
    | Error(err) -> Error(err)
  )

  | ERet(ET(EFun(x,e))) ->
      Ok(ET(EClosure(x,e, List.hd enstk)), enstk)
  | ERet(ET(e)) -> Ok(ET(e), List.tl enstk)
  | ERet(ENT(e)) -> (match trace1_expr e enstk with
    | Ok(e', enstk') -> Ok(ENT(ERet(e')), enstk')
    | Error(err) -> Error(err)
  )

  | EThunk(exp, saved) -> Ok((ENT(ERet(exp)), saved::enstk))

  | EName(ide) -> (match (List.hd enstk) ide with
    | Some(e) -> Ok(e, enstk)
    | None -> Error("variable outside of scope"))

  | ELetIn(ide, e1, e2) -> Ok(ENT(EApp(ET(EFun(ide, e2)), e1)), enstk)

  (* binary op with only rhs not terminal *)
  | EBPrim(ET(c1), bop, ENT(e2)) -> (match trace1_expr e2 enstk with
    | Ok(e2', enstk') -> Ok(ENT(EBPrim(ET(c1), bop, e2')), enstk')
    | Error(err) -> Error(err)
  )

  (* binary op with lhs not terminal *)
  | EBPrim(ENT(e1), bop, e2) -> (match trace1_expr e1 enstk with
    | Ok(e1', enstk') -> Ok(ENT(EBPrim(e1', bop, e2)), enstk')
    | Error(err) -> Error(err)
  )

  (* binary op with lhs and rhs terminal*)
  | EBPrim(ET(c1), bop, ET(c2)) -> (match (c1,bop,c2) with
    | (CNum(n1), BOPlus, CNum(n2)) -> Ok(ET(CNum(n1+n2)), enstk)
    | (CNum(n1), BOMinus, CNum(n2)) -> Ok(ET(CNum(n1-n2)), enstk)
    | (CNum(n1), BOTimes, CNum(n2)) -> Ok(ET(CNum(n1*n2)), enstk)

    | (CBool(b1), BOAnd, CBool(b2)) -> Ok(ET(CBool(b1 && b2)), enstk)
    | (CBool(b1), BOOr, CBool(b2)) -> Ok(ET(CBool(b1 || b2)), enstk)

    | (CNum(n1), BOLeq, CNum(n2)) -> Ok(ET(CBool(n1 <= n2)), enstk)
    | (CNum(n1), BOIEq, CNum(n2)) -> Ok(ET(CBool(n1 = n2)), enstk)
    | (CBool(b1), BOBEq, CBool(b2)) -> Ok(ET(CBool(b1 = b2)), enstk)

    | (_, op, _) -> Error("Type mismatch for operator " ^ (string_of_binop op))
  )

  (* 'not' with e not terminal*)
  | EUPrim(UONot, ENT(e)) -> (match trace1_expr e enstk with
    | Ok(e', enstk') -> Ok(ENT(EUPrim(UONot, e')), enstk')
    | Error(err) -> Error(err)
  )

  (* 'not' with boolean terminal *)
  | EUPrim(UONot, ET(CBool(b))) -> Ok(ET(CBool(not b)), enstk)
  | EUPrim(UONot, ET(_)) -> Error("Type mismatch for 'not'")
  ;;

let rec eval_expr (max: int) (e: expr) (enstk: env list): (expr, expr*string) result =
  match e with
  | ET e -> Ok(ET e)
  | ENT e -> (
    if max=0 then Ok (ENT e) else
    match trace1_expr e enstk with
    | Ok (ENT e', enstk') -> eval_expr (max-1) (ENT e') enstk'
    | Ok (ET e', _) -> Ok(ET e')
    | Error(err) -> Error(ENT e,err)
  )

type evalerr =
  | ProgCheckErr of progCheckError list
  | RuntimeErr of expr * string

let eval_prog (max: int) ((tl, dl, ex): program) : (expr, evalerr) result =
  match typecheck_prog (tl,dl,ex) with
    | Error(errlist) -> Error(ProgCheckErr(errlist))
    | Ok(_) -> (
      let outer_env = 
        List.fold_left (fun env (ide,expr) -> bind env (NVar ide) (Some expr)) static_env dl
      in match eval_expr max ex [outer_env] with
        | Ok(e) -> Ok e
        | Error(e,s) -> Error(RuntimeErr(e, s))
    )
;;
  