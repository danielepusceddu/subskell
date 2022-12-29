open Ast

let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec typecheck typenv = function
  | EConst(CNum _)
  | EBinOp(_, BOPlus, _)
  | EBinOp(_, BOTimes, _)
  | EBinOp(_, BOMinus, _) -> TInt

  | EConst(CBool _)
  | EBinOp(_, BOAnd, _)
  | EBinOp(_, BOOr, _)
  | EUnOp(UONot, _) -> TBool

  | EFun(x, expr) -> TFun(typenv x, typecheck typenv expr)
  | EApp(e1, e2) -> (
      let funtype = typecheck typenv e1 in
      match funtype with
        | TFun(t1, t2) -> (
            let paramtype = typecheck typenv e2 in
            if t1 != paramtype then failwith "type error" else t2
        )
        | _ -> failwith "e1 not a function"
  )
  | EIf(e1, e2, e3) -> 
      if typecheck typenv e1 != TBool then failwith "Not boolean" else 
        let t2 = typecheck typenv e2 in
        let t3 = typecheck typenv e3 in
        if t2 != t3 then failwith "type error" else t2
  | EVar x -> typenv x
  ;;

let rec respects_sig (definition: expr) (tsig: typing) (tenv: typenv) =
  match (definition, tsig) with
    | (EFun(x, expr), TFun(t1,t2)) ->
        (* assign to each subsequent binder the leftmost terminal type of the signature *)
        let tenv' y = if x=y then t1 else tenv y in
        respects_sig expr t2 tenv'

    | (def, typ) -> (typecheck tenv def) = typ

let bind f x v = fun y -> if x=y then v else f y

let rec typenv_of_typesigs = function
  | [] -> tbottom
  | (ide,ty)::t -> bind (typenv_of_typesigs t) ide ty

let typecheck_prog (ts,ds,main) =
  let (tenv: typenv) = typenv_of_typesigs ts in

  assert (List.fold_left
    (fun b (ide,def) -> b && respects_sig def (tenv ide) tenv)
    true
    ds
  );

  typecheck tenv main
