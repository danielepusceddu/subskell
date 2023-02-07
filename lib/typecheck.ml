open Ast
open Prettyprint

type typenv = (ide -> typing option)

let bind f x v = fun y -> if x=y then v else f y

let is_arithop = function
    BOPlus | BOTimes | BOMinus -> true
  | _ -> false

let is_boolop = function
    BOAnd | BOOr -> true
  | _ -> false

let rec typecheck (typenv: typenv) = function
  | ET(e) -> (match e with
    | CNum _ -> Ok TInt
    | CBool _ -> Ok TBool
    | EFun(x, expr) -> (match typecheck typenv expr with 
      | Ok(t) -> (match typenv x with 
          | Some(t2) -> (Ok (TFun(t2, t)))
          | None -> Error "Unknown type of function parameter")
      | Error(e) -> Error e
    )
    | EClosure(_) -> Error("EClosure should be runtime only")
  )

  | ENT(e) -> (match e with
    | EBinOp(e1, BOEq, e2) -> (
      let (r1,r2) = (typecheck typenv e1, typecheck typenv e2)
      in match (r1,r2) with
      | (Ok t1, Ok t2) when t1=t2 -> Ok TBool
      | (Ok _, Ok _) -> Error("Equality between different types")
      | (Error err, Ok _)
      | (Ok _, Error err) -> Error(err)
      | (Error err1, Error err2) -> Error (err1 ^ " " ^ err2)
    )

    | EBinOp(e1, BOLeq, e2) -> (
      let (r1,r2) = (typecheck typenv e1, typecheck typenv e2)
      in match (r1,r2) with
      | (Ok TInt, Ok TInt) -> Ok TBool
      | (Ok _, Ok _) -> Error("Ordering between types different than integers")
      | (Error err, Ok _)
      | (Ok _, Error err) -> Error(err)
      | (Error err1, Error err2) -> Error (err1 ^ " " ^ err2)
    )
  
    | EBinOp(e1, op, e2) when is_arithop op -> (
      if typecheck typenv e1 = Ok TInt && typecheck typenv e2 = Ok TInt 
      then Ok TInt
      else Error ("Ill-typed operand on " ^ string_of_binop op)
    )
  
    | EBinOp(e1, op, e2) -> (
      if typecheck typenv e1 = Ok TBool && typecheck typenv e2 = Ok TBool 
      then Ok TBool
      else Error ("Ill-typed operand on " ^ string_of_binop op)
    )
  
    | EUnOp(UONot, e) -> (
      if typecheck typenv e = Ok TBool
      then Ok TBool
      else Error ("Ill-typed operand on " ^ string_of_unop UONot)
    )
  
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
  
    | ELetIn(x, e1, e2) -> (match typecheck typenv e1 with
      | Ok(t) -> typecheck (bind typenv x (Some(t))) (ENT(EApp(ET(EFun(x, e2)), e1)))
      | Error(err) -> Error(err)
    )
  
    | EVar x -> (match typenv x with
      | Some t -> Ok t
      | None -> Error ("Identifier " ^ x ^ " with no type"))
    | ERet(_) -> Error("ERet should be runtime only")
    | EThunk(_) -> Error("EThunk should be runtime only")
  
  )
;;

type sigError = DifferentType of typing * typing | TypingError of string
let rec respects_sig (definition: expr) (tsig: typing) (tenv: typenv) =
  match (definition, tsig) with
    | (ET(EFun(x, expr)), TFun(t1,t2)) ->
        (* assign to each subsequent binder the leftmost terminal type of the signature *)
        let tenv' y = if x=y then Some t1 else tenv y in
        respects_sig expr t2 tenv'

    | (def, typ) -> (match typecheck tenv def with
        | Ok(t) -> if t = typ then Ok () else Error(DifferentType (typ,t))
        | Error(err) -> Error(TypingError err)
      )

let rec typenv_of_typesigs = function
  | [] -> tbottom
  | (ide,ty)::t -> bind (typenv_of_typesigs t) ide (Some ty)

type progCheckError = 
  | NoTypeSignature of ide
  | SigError of ide * sigError
  | TypecheckError of string

let typecheck_prog ((ts,ds,main): program) : (typing, progCheckError list) result =
  let (tenv: typenv) = typenv_of_typesigs ts in
  let tsigerrlist = (List.fold_left
    (fun l (ide,def) -> match tenv ide with 
      | None -> (NoTypeSignature ide)::l
      | Some t -> (match respects_sig def t tenv with
        | Ok() -> l
        | Error(err) -> (SigError(ide, err))::l
      )
    )
    []
    ds) in
  let typing = typecheck tenv main in
  match (tsigerrlist,typing) with
    | ([],Ok(t)) -> Ok(t)
    | (l, Ok(_)) -> Error l
    | (l, Error(err)) -> Error((TypecheckError(err))::l)
;;