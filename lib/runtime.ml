open Typescommon
open Typesruntime

type envstack = env list

(* The environment with the definitions of predefined names. *)
let static_env : env = function
  (* For every operator, 
     associate a wrapper function for its Prim implementation *)
  | NBop(op) ->  Some(ET(
      EFun("a", (ET(EFun("b", 
      ENT(EBPrim(ENT(EName(NVar "a")), op, ENT(EName(NVar "b")))))
      )))))
  | NUop(op) ->  Some(ET(EFun("a", ENT(EUPrim(op, ENT(EName(NVar "a")))))))

  (* Other names are not present in the initial environment *)
  | NVar(_) -> None
;;

let bind (f: env) (x:ide) (e: expr) = 
  fun x' -> if x'=(NVar x) then Some(e) else f x'

(* Splitting expressions into terminal and not terminal allows us to simplify the
   type of this smallstep function: it takes only non-terminal expressions.
   Theoretically, if the AST has been typechecked properly, this function should never fail.
   However, the ocaml types we are using are not powerful enough to represent 
   concepts such as "an expression that is guaranteed to evaluate to int",
   and therefore in our pattern matching I have to include errors that are theoretically impossible. 
   I have attempted looking into GADTs to solve this issue, but found it difficult to extend their use
   beyond a simple arithmetic language. https://dev.realworldocaml.org/gadts.html *)
let rec trace1_expr (ex: nonterm) (enstk: env list) : (expr*env list, string) result = match ex with
  (* Application of a closure:
     turn the app rhs into a thunk to be evaluated inside expr1 using 
     the saved environment of the closure with the thunk binded to the function argument *)
  | EApp(ET(EClosure(ide, expr1, saved)), expr2) ->
      let param = ENT(EThunk(expr2, List.hd enstk)) in
      let env = bind saved ide param in
      Ok(ENT(ERet expr1), env::enstk)

  (* Application of a function:
     same as the closure case, but use the current environment. *)
  | EApp(ET(EFun(ide, expr1)), expr2) ->
      let param = ENT(EThunk(expr2, List.hd enstk)) in
      let env = bind (List.hd enstk) ide param in
      Ok (ENT(ERet expr1), env::enstk)

  (* Application of some other terminal expression:
     Only functions and closures are callable. *)
  | EApp(ET(_), _) -> Error("lhs of apply not a function")

  (* Application of non-terminal lhs:
     Do a step of lhs *)
  | EApp(ENT(expr1), expr2) -> (
      match trace1_expr expr1 enstk with
      | Ok(expr1', en') -> Ok(ENT(EApp(expr1', expr2)), en')
      | Error(err) -> Error(err)
  )

  (* If with terminal, boolean e1: pick the expression based on the value *)
  | EIf(ET(CBool(true)), e, _)
  | EIf(ET(CBool(false)), _, e) -> Ok(e, enstk)

  (* If with terminal, non-boolean e1: undefined *)
  | EIf(ET(_), _, _) -> Error("trace: if without a boolean condition")

  (* If with non-terminal e1: do a step *)
  | EIf(ENT(e1), e2, e3) -> (match trace1_expr e1 enstk with
    | Ok(e1', enstk') -> Ok(ENT(EIf(e1', e2, e3)), enstk')
    | Error(err) -> Error(err)
  )

  (* Ret of a function: turn it into a closure, saving the env we're currently using.
     Remove that env from the stack. *)
  | ERet(ET(EFun(x,e))) ->
      Ok(ET(EClosure(x,e, List.hd enstk)), List.tl enstk)

  (* Ret of other terminal expressions: turn into that expression,
     and pop the environment stack. *)
  | ERet(ET(e)) -> Ok(ET(e), List.tl enstk)

  (* Ret of non-terminal expressions: do a step. *)
  | ERet(ENT(e)) -> (match trace1_expr e enstk with
    | Ok(e', enstk') -> Ok(ENT(ERet(e')), enstk')
    | Error(err) -> Error(err)
  )

  (* Thunks are treated as closures without parameters needed. *)
  | EThunk(exp, saved) -> Ok((ENT(ERet(exp)), saved::enstk))

  (* Names are looked up in the environment currently in use. *)
  | EName(ide) -> (match (List.hd enstk) ide with
    | Some(e) -> Ok(e, enstk)
    | None -> Error((string_of_name ide) ^ " variable outside of scope"))

  (* Let-Ins are turned into calls of a function EFun(ide, e2)
     with argument e1*)
  | ELetIn(ide, e1, e2) -> (* permit recursion with let in *)
    let saved = bind (List.hd enstk) ide e1 in
    Ok(ENT(EApp(ET(EFun(ide, e2)), ENT(EThunk(e1,saved)))), enstk)

  (* Binary Primitive with lhs terminal and rhs not terminal: do a step of rhs *)
  | EBPrim(ET(c1), bop, ENT(e2)) -> (match trace1_expr e2 enstk with
    | Ok(e2', enstk') -> Ok(ENT(EBPrim(ET(c1), bop, e2')), enstk')
    | Error(err) -> Error(err)
  )

  (* Binary Primitive with lhs not terminal: do a step of lhs *)
  | EBPrim(ENT(e1), bop, e2) -> (match trace1_expr e1 enstk with
    | Ok(e1', enstk') -> Ok(ENT(EBPrim(e1', bop, e2)), enstk')
    | Error(err) -> Error(err)
  )

  (* Binary Primitive with lhs and rhs terminal*)
  | EBPrim(ET(c1), bop, ET(c2)) -> (match (c1,bop,c2) with
    | (CNum(n1), BOPlus, CNum(n2)) -> Ok(ET(CNum(n1+n2)), enstk)
    | (CNum(n1), BOMinus, CNum(n2)) -> Ok(ET(CNum(n1-n2)), enstk)
    | (CNum(n1), BOTimes, CNum(n2)) -> Ok(ET(CNum(n1*n2)), enstk)

    | (CBool(b1), BOAnd, CBool(b2)) -> Ok(ET(CBool(b1 && b2)), enstk)
    | (CBool(b1), BOOr, CBool(b2)) -> Ok(ET(CBool(b1 || b2)), enstk)

    | (CNum(n1), BOLeq, CNum(n2)) -> Ok(ET(CBool(n1 <= n2)), enstk)
    | (CNum(n1), BOEq, CNum(n2)) -> Ok(ET(CBool(n1 = n2)), enstk)
    | (CBool(b1), BOEq, CBool(b2)) -> Ok(ET(CBool(b1 = b2)), enstk)

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
;;