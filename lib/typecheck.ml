open Ast
open Help

module NameMap = Map.Make(struct type t = name let compare = compare end);;
type tenv = typing NameMap.t;;

let static_tenv : tenv = List.fold_left 
  (fun acc (key,v) -> NameMap.add key v acc) 

  NameMap.empty

  [(NBop(BOPlus), TFun(TInt, TFun(TInt, TInt)));
   (NBop(BOMinus), TFun(TInt, TFun(TInt, TInt)));
   (NBop(BOTimes), TFun(TInt, TFun(TInt, TInt)));
   
   (NBop(BOAnd), TFun(TBool, TFun(TBool, TBool)));
   (NBop(BOOr), TFun(TBool, TFun(TBool, TBool)));
   (NBop(BOBEq), TFun(TBool, TFun(TBool, TBool)));
   
   (NBop(BOLeq), TFun(TInt, TFun(TInt, TBool)));
   (NBop(BOIEq), TFun(TInt, TFun(TInt, TBool)));
   
   (NUop(UONot), TFun(TBool, TBool))
  ]

let tbind : (name -> typing -> tenv -> tenv) = NameMap.add
let tlookup : (name -> tenv -> typing option) = NameMap.find_opt

type constr = typing * typing;;
module ConstrSet = Set.Make(struct type t = constr let compare = compare end);;
let add_mul = List.fold_right ConstrSet.add;;
let empty = ConstrSet.empty;;
let union_mul = List.fold_right ConstrSet.union;;

let rec getconstrs (env: tenv) (max_tvar: int) = function
  | ET(e) -> (match e with
    | CNum _ -> (TInt, max_tvar, empty)
    | CBool _ -> (TBool, max_tvar, empty)
    | EFun(x,e) -> 
      let fresh_i = max_tvar+1 in
      let fresh = TVar fresh_i in
      let newenv = tbind (NVar x) fresh env in
      let (t2, max, c) = getconstrs newenv fresh_i e in
      (TFun(fresh, t2), max, c)

    | EClosure(_,_,_) -> failwith "EClosure should be runtime only"
  )

  | ENT(e) -> (match e with
    | EName(x) -> (match tlookup x env with
      | Some(t) -> (t, max_tvar, empty)
      | None -> failwith "name without type"
    )

    | EIf(e1, e2, e3) -> 
      let fresh_i = 1+max_tvar in
      let fresh = TVar fresh_i in
      let (t1,max,c1) = getconstrs env fresh_i e1 in
      let (t2,max,c2) = getconstrs env max e2 in 
      let (t3,max,c3) = getconstrs env max e3 in
      let c = add_mul [(t1, TBool); (fresh, t2); (fresh, t3)] empty in
      let united = union_mul [c1; c2; c3] c in
      (fresh, max, united)

    | EApp(e1, e2) -> 
      let fresh_i = 1+max_tvar in 
      let fresh = TVar fresh_i in
      let (t1,max,c1) = getconstrs env fresh_i e1 in
      let (t2,max,c2) = getconstrs env max e2 in 
      let c = ConstrSet.add ((t1, TFun(t2, fresh))) empty in
      let united = union_mul [c1;c2] c in
      (fresh, max, united)

    | ELetIn(x,e1,e2) -> (* modified to allow recursion *)
      let fresh_i = 1+max_tvar in 
      let fresh = TVar fresh_i in
      let env' = tbind (NVar x) fresh env in
      let (t1,max,c1) = getconstrs env' fresh_i e1 in
      let env' = tbind (NVar x) t1 env in
      let (t2,max,c2) = getconstrs env' max e2 in
      let united = ConstrSet.union c1 c2 in
      (t2,max,united)

    | ERet(_)
    | EThunk(_,_)
    | EUPrim(_,_)
    | EBPrim(_,_,_) -> failwith "Runtime only"
  )
;;

type tsubst = int * typing
let rec dotsubst ((tvar,newt):tsubst) (t:typing) =
  match (tvar, newt, t) with
  | (_,_,TInt) -> TInt
  | (_,_,TBool) -> TBool
  | (x,t',TVar y) when x=y -> t'
  | (_,_,TVar y) -> TVar y
  | (x,t',TFun(t1,t2)) -> TFun(dotsubst (x, t') t1, dotsubst (x, t') t2)
let dotsubsts = List.fold_right dotsubst;;

let docsubst (s: tsubst) ((t1,t2): constr) =
  (dotsubst s t1, dotsubst s t2)
let docsubsts = List.fold_right docsubst;;

let is_unifying (s: tsubst) ((t1,t2): constr) =
  (dotsubst s t1) = (dotsubst s t2)

let rec toccurs (x: int) (t: typing) : bool = match t with
  | TVar y when x=y -> true
  | TVar _
  | TBool
  | TInt -> false
  | TFun(t1,t2) -> (toccurs x t1) || (toccurs x t2)

let unify (constrset: ConstrSet.t) : (tsubst list, constr list) result = 
  let rec helper = function
    | [] -> Ok([])
    | (TInt, TInt)::t
    | (TBool, TBool)::t -> helper t
    | (TVar x, TVar y)::t when x=y -> helper t

    | (TVar x, ty)::t when (not (toccurs x ty)) -> (
      match helper (List.map (docsubst (x,ty)) t) with
      | Ok(substs) -> Ok((x,ty)::substs)
      | Error(_) as err -> err
    )
    | (ty, TVar x)::t when (not (toccurs x ty)) -> (
      match helper (List.map (docsubst (x,ty)) t) with
      | Ok(substs) -> Ok((x,ty)::substs)
      | Error(_) as err -> err
    )
    | (TFun(i1,o1),TFun(i2,o2))::t -> helper((i1,i2)::(o1,o2)::t)

    | l -> Error(l)
in helper (ConstrSet.to_seq constrset |> List.of_seq)

let tinfer_expr (env: tenv) (e: expr) = 
  let (t,_,constrs) = getconstrs env (-1) e in
  match unify constrs with
  | Ok(substs) -> Ok(dotsubsts substs t)
  | Error(_) as err -> err

(*Input: types such as 'hello -> 'wow -> 'hello
  Output: 'a -> 'b -> 'a *)
let norm_vars t = 
  let rec helper (next:int) m = (function
    | TInt -> (TInt, next, m)
    | TBool -> (TBool, next, m)
    | TVar x -> (match IntMap.find_opt x m with
      | Some y -> (TVar y, next, m)
      | None -> (TVar next, next+1, IntMap.add x next m))
    | TFun(t1,t2) -> 
      let (t1', next', m') = helper next m t1 in
      let (t2', next'', m'') = helper next' m' t2 in
      (TFun(t1',t2'), next'', m'')
  ) 
  in let (t',_,_) = helper 0 IntMap.empty t
  in t'
  
let types_equal t1 t2 = (norm_vars t1) = (norm_vars t2)

type progCheckError = 
  | UnsatConstr of ide * (constr list)
  | DifferentType of ide * typing * typing

let rec get_typenv (l: (ide*expr*(typing option)) list) : (tenv,progCheckError) result =
  match l with
  | [] -> Ok static_tenv
  | (ide,e,Some(tsig))::t -> (match get_typenv t with
    | Ok(tenv) -> (match tinfer_expr tenv e with
      | Ok(t) when types_equal tsig t -> Ok(tbind (NVar ide) tsig tenv)
      | Ok(t) -> Error(DifferentType(ide, tsig, t))
      | Error(constrl) -> Error(UnsatConstr(ide,constrl))
    )
    | Error(_) as err -> err
  )
  | (ide,e,None)::t -> (match get_typenv t with 
    | Ok(tenv) -> (match tinfer_expr tenv e with
      | Ok(t) -> Ok(tbind (NVar ide) t tenv)
      | Error(constrl) -> Error(UnsatConstr(ide,constrl)))
    | Error(_) as err -> err
  )

let typecheck_prog ((ts,ds,main): program) =
  let zipped = List.map
  (fun (ide,e) -> 
    let tsig = List.find_map (fun (ide2,t) -> if ide=ide2 then Some t else None) ts
    in (ide, e, tsig))
  ds in let zipped = List.rev zipped
  in match get_typenv zipped with
    | Ok(tenv) -> (match tinfer_expr tenv main with
      | Ok(t) -> Ok(t, tenv)
      | Error(constrs) -> Error(UnsatConstr("main",constrs)))
    | Error(_) as err -> err
;;