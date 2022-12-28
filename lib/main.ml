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

let respects_sig (definition: expr) (tsig: typing) (tenv) =
  match (definition, tsig) with
    | (EFun(x, expr), TFun(t1,t2)) -> 
        let tenv' y = if x=y then t1 else tenv y in
        (typecheck tenv' expr) = t2

    | (EFun(_,_), _) -> false

    | (def, typ) -> (typecheck tenv def) = typ
;;