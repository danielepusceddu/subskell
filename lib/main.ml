open Ast
open Typecheck

let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bind f x e x' = if x'=x then e else f x'

let rec trace1_expr (ex: expr) (enstk: env list) : ((expr*env list) option, string) result = match ex with
  | EConst(_)
  | EFun(_, _) -> Ok None
  | EClosure(_) -> Ok None

  | EApp(EClosure(ide, expr1, saved), expr2) ->
      Ok(Some(ERet expr1, (bind saved ide (Some(EThunk(expr2, List.hd enstk))))::enstk))
  | EApp(EFun(ide, expr1), expr2) ->
      Ok (Some(ERet expr1, (bind (List.hd enstk) ide (Some(EThunk(expr2, List.hd enstk))))::enstk))
  | EApp(expr1, expr2) -> (
      match trace1_expr expr1 enstk with
      | Ok(None) -> Error("lhs of apply not a function")
      | Ok(Some(expr1', en')) -> Ok(Some(
          EApp(expr1', expr2), en'))
      | Error(err) -> Error(err)
  )

  | EIf(EConst(CBool(true)), e, _)
  | EIf(EConst(CBool(false)), _, e) -> 
      Ok(Some(e, enstk))
  | EIf(e1, e2, e3) -> (match trace1_expr e1 enstk with
    | Ok(None) -> Error("if without a boolean")
    | Ok(Some(e1', enstk')) -> Ok(Some(EIf(e1', e2, e3), enstk'))
    | Error(err) -> Error(err)
  )

  | ERet(EFun(x,e)) ->
      Ok(Some(EClosure(x,e, List.hd enstk), enstk))
  | ERet(e) -> (match trace1_expr e enstk with
    | Ok None -> Ok(Some(e, List.tl enstk))
    | Ok(Some(e', enstk')) -> Ok(Some(ERet(e'), enstk'))
    | Error(err) -> Error(err)
  )

  | EThunk(exp, saved) -> Ok(Some((ERet(exp), saved::enstk)))

  | EVar(ide) -> (match (List.hd enstk) ide with
    | Some(e) -> Ok(Some(e, enstk))
    | None -> Error("variable outside of scope"))
    
  | ELetIn(ide, e1, e2) -> Ok(Some(EApp(EFun(ide, e2), e1), enstk))
    
    (* boring bs starts here *)
    (* less eq: terminal *)
    | EBinOp(EConst(CNum(n1)), BOLeq, EConst(CNum(n2))) -> Ok(Some(EConst(CBool(n1<=n2)), enstk))

    (* equality: rhs not an integer *)
    | EBinOp(EConst(CNum(n1)), BOLeq, e2) -> (match trace1_expr e2 enstk with
      | Ok(None) -> Error("rhs of <= not an integer")
      | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CNum(n1)), BOLeq, e2'), enstk'))
      | Error(err) -> Error(err)
    )

    (* equality: lhs and rhs not integers*)
    | EBinOp(e1, BOLeq, e2) -> (match trace1_expr e1 enstk with
      | Ok(None) -> Error("lhs of <= not an integer")
      | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOLeq, e2), enstk'))
      | Error(err) -> Error(err)
    )

    (* equality: terminal *)
    | EBinOp(EConst(CNum(n1)), BOEq, EConst(CNum(n2))) -> Ok(Some(EConst(CBool(n1=n2)), enstk))
    | EBinOp(EConst(CBool(b1)), BOEq, EConst(CBool(b2))) -> Ok(Some(EConst(CBool(b1=b2)), enstk))

    (* equality: constants of different types / functions *)
    | EBinOp(EConst(_), BOEq, EConst(_)) -> Error("Equality between different types or between functions")

    (* equality: rhs not a constant*)
    | EBinOp(EConst(c), BOEq, e2) -> (match trace1_expr e2 enstk with
      | Ok(None) -> Error("wat")
      | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(c), BOEq, e2'), enstk'))
      | Error(err) -> Error(err)
    )

    (* equality: lhs and rhs not a constant*)
    | EBinOp(e1, BOEq, e2) -> (match trace1_expr e1 enstk with
      | Ok(None) -> Error("wat")
      | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOEq, e2), enstk'))
      | Error(err) -> Error(err)
    )

    | EBinOp(EConst(CNum(n1)), BOPlus, EConst(CNum(n2))) -> Ok(Some(EConst(CNum(n1+n2)), enstk))
    | EBinOp(EConst(CNum(n1)), BOPlus, e2) -> (match trace1_expr e2 enstk with
      | Ok(None) -> Error("rhs of + is not an integer")
      | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CNum(n1)), BOPlus, e2'), enstk'))
      | Error(err) -> Error(err)
    )
    | EBinOp(e1, BOPlus, e2) -> (match trace1_expr e1 enstk with
      | Ok(None) -> Error("lhs of + is not an integer")
      | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOPlus, e2), enstk'))
      | Error(err) -> Error(err)
    )

  | EBinOp(EConst(CNum(n1)), BOMinus, EConst(CNum(n2))) -> Ok(Some(EConst(CNum(n1-n2)), enstk))
  | EBinOp(EConst(CNum(n1)), BOMinus, e2) -> (match trace1_expr e2 enstk with
    | Ok(None) -> Error("rhs of - is not an integer")
    | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CNum(n1)), BOMinus, e2'), enstk'))
    | Error(err) -> Error(err)
  )
  | EBinOp(e1, BOMinus, e2) -> (match trace1_expr e1 enstk with
    | Ok(None) -> Error("lhs of - is not an integer")
    | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOMinus, e2), enstk'))
    | Error(err) -> Error(err)
  )

  | EBinOp(EConst(CNum(n1)), BOTimes, EConst(CNum(n2))) -> Ok(Some(EConst(CNum(n1*n2)), enstk))
  | EBinOp(EConst(CNum(n1)), BOTimes, e2) -> (match trace1_expr e2 enstk with
    | Ok(None) -> Error("rhs of * is not an integer")
    | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CNum(n1)), BOTimes, e2'), enstk'))
    | Error(err) -> Error(err)
  )
  | EBinOp(e1, BOTimes, e2) -> (match trace1_expr e1 enstk with
    | Ok(None) -> Error("lhs of * is not an integer")
    | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOTimes, e2), enstk'))
    | Error(err) -> Error(err)
  )
  
  | EBinOp(EConst(CBool(b1)), BOAnd, EConst(CBool(b2))) -> Ok(Some(EConst(CBool(b1 && b2)), enstk))
  | EBinOp(EConst(CBool(b1)), BOAnd, e2) -> (match trace1_expr e2 enstk with
    | Ok(None) -> Error("rhs of && is not an integer")
    | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CBool(b1)), BOAnd, e2'), enstk'))
    | Error(err) -> Error(err)
  )
  | EBinOp(e1, BOAnd, e2) -> (match trace1_expr e1 enstk with
    | Ok(None) -> Error("lhs of && is not an integer")
    | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOAnd, e2), enstk'))
    | Error(err) -> Error(err)
  )

  | EBinOp(EConst(CBool(b1)), BOOr, EConst(CBool(b2))) -> Ok(Some(EConst(CBool(b1 || b2)), enstk))
  | EBinOp(EConst(CBool(b1)), BOOr, e2) -> (match trace1_expr e2 enstk with
    | Ok(None) -> Error("rhs of || is not an integer")
    | Ok(Some(e2', enstk')) -> Ok(Some(EBinOp(EConst(CBool(b1)), BOOr, e2'), enstk'))
    | Error(err) -> Error(err)
  )
  | EBinOp(e1, BOOr, e2) -> (match trace1_expr e1 enstk with
    | Ok(None) -> Error("lhs of || is not an integer")
    | Ok(Some(e1', enstk')) -> Ok(Some(EBinOp(e1', BOOr, e2), enstk'))
    | Error(err) -> Error(err)
  )

  | EUnOp(UONot, EConst(CBool(b))) -> Ok(Some(EConst(CBool(not b)), enstk))
  | EUnOp(UONot, e) -> (match trace1_expr e enstk with
    | Ok(None) -> Error("operand of 'not' is not an integer")
    | Ok(Some(e', enstk')) -> Ok(Some(EUnOp(UONot, e'), enstk'))
    | Error(err) -> Error(err)
  )

let rec eval_expr (max: int) (e: expr) (enstk: env list): (expr, expr*string) result =
  if max=0 then Ok e else
  match trace1_expr e enstk with
  | Ok None -> Ok e
  | Ok (Some(e', enstk')) -> eval_expr (max-1) e' enstk'
  | Error(err) -> Error(e,err)

type evalerr =
  | ProgCheckErr of progCheckError list
  | RuntimeErr of expr * string

let eval_prog (max: int) ((tl, dl, ex): program) : (expr, evalerr) result =
  match typecheck_prog (tl,dl,ex) with
    | Error(errlist) -> Error(ProgCheckErr(errlist))
    | Ok(_) -> (
      let outer_env = 
        List.fold_left (fun env (ide,expr) -> bind env ide (Some expr)) bottom dl
      in match eval_expr max ex [outer_env] with
        | Ok(e) -> Ok e
        | Error(e,s) -> Error(RuntimeErr(e, s))
    )
;;
  