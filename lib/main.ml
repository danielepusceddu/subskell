open Ast

let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

type typenv = (ide -> typing option)
let rec typecheck (typenv: typenv) = function
  | EConst(CNum _)
  | EBinOp(_, BOPlus, _)
  | EBinOp(_, BOTimes, _)
  | EBinOp(_, BOMinus, _) -> Ok TInt

  | EConst(CBool _)
  | EBinOp(_, BOAnd, _)
  | EBinOp(_, BOOr, _)
  | EUnOp(UONot, _) -> Ok TBool

  | EFun(x, expr) -> (match typecheck typenv expr with 
      | Ok(t) -> (match typenv x with 
          | Some(t2) -> (Ok (TFun(t2, t)))
          | None -> Error "Unknown type of function parameter")
      | Error(e) -> Error e)

  | EApp(e1, e2) -> (
      let funtype = typecheck typenv e1 in
      match funtype with
        | Ok(TFun(t1, t2)) -> (match typecheck typenv e2 with
            | Ok(t) when t1=t -> Ok t2
            | Ok(_) -> Error "rhs of function application has the wrong type"
            | Error(err) -> Error(err)
        )
        | Ok _ -> Error "lhs of function application is not a function"
        | Error err -> Error err
  )
  | EIf(e1, e2, e3) ->(match typecheck typenv e1 with
    | Error(err) -> Error err
    
    | Ok(TBool) -> (match typecheck typenv e2 with
      | Error(err) -> Error err
      | Ok(t2) -> (match typecheck typenv e3 with
      | Error(err) -> Error(err)
      | Ok(t3) when t2=t3 -> Ok t3
      | Ok(_) -> Error "If: type ambiguity"
      )
    )
    | Ok(_) -> Error "If: condition is not a boolean"
  )
  | EVar x -> match typenv x with
    | Some t -> Ok t
    | None -> Error ("Identifier " ^ x ^ " with no type")
  ;;

type sigError = DifferentType of typing * typing | TypingError of string
let rec respects_sig (definition: expr) (tsig: typing) (tenv: typenv) =
  match (definition, tsig) with
    | (EFun(x, expr), TFun(t1,t2)) ->
        (* assign to each subsequent binder the leftmost terminal type of the signature *)
        let tenv' y = if x=y then Some t1 else tenv y in
        respects_sig expr t2 tenv'

    | (def, typ) -> (match typecheck tenv def with
        | Ok(t) -> if t = typ then Ok () else Error(DifferentType (typ,t))
        | Error(err) -> Error(TypingError err)
      )

let bind f x v = fun y -> if x=y then v else f y

let rec typenv_of_typesigs = function
  | [] -> tbottom
  | (ide,ty)::t -> bind (typenv_of_typesigs t) ide (Some ty)

type progCheckError = NoTypeSignature | SigError of sigError
let typecheck_prog ((ts,ds,main): program) =
  let (tenv: typenv) = typenv_of_typesigs ts in
  let errlist = (List.fold_left
    (fun l (ide,def) -> match tenv ide with 
      | None -> (ide, NoTypeSignature)::l
      | Some t -> (match respects_sig def t tenv with
        | Ok() -> l
        | Error(err) -> (ide, SigError err)::l
      )
    )
    []
    ds) in
  match errlist with
    | [] -> Ok(typecheck tenv main)
    | l -> Error(l)
