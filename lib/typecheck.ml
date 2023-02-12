open Ast
open Help

module NameMap = Map.Make(struct type t = name let compare = compare end);;
type tenv = tscheme NameMap.t;;

let static_tenv : tenv = List.fold_left 
  (fun acc (key,v) -> NameMap.add key v acc) 

  NameMap.empty

  [(NBop(BOPlus), ([], TFun(TInt, TFun(TInt, TInt))));
   (NBop(BOMinus), ([], TFun(TInt, TFun(TInt, TInt))));
   (NBop(BOTimes), ([], TFun(TInt, TFun(TInt, TInt))));
   
   (NBop(BOAnd), ([], TFun(TBool, TFun(TBool, TBool))));
   (NBop(BOOr), ([], TFun(TBool, TFun(TBool, TBool))));
   (NBop(BOBEq), ([], TFun(TBool, TFun(TBool, TBool))));
   
   (NBop(BOLeq), ([], TFun(TInt, TFun(TInt, TBool))));
   (NBop(BOIEq), ([], TFun(TInt, TFun(TInt, TBool))));
   
   (NUop(UONot), ([], TFun(TBool, TBool)))
  ]

let tbind : (name -> tscheme -> tenv -> tenv) = NameMap.add
let tlookup : (name -> tenv -> tscheme option) = NameMap.find_opt

type constr = typing * typing;;
module ConstrSet = Set.Make(struct type t = constr let compare = compare end);;
let add_mul = List.fold_right ConstrSet.add;;
let empty = ConstrSet.empty;;
let union_mul = List.fold_right ConstrSet.union;;

type tsubst = int * typing
let rec dotsubst ((tvar,newt):tsubst) ((l,t):tscheme) =
  match (tvar, newt, l, t) with
  | (_,_,_,TInt) -> ([],TInt)
  | (_,_,_,TBool) -> ([],TBool)
  | (x,t',l,TVar y) when x=y && (not (List.mem x l)) -> ([],t')
  | (x,_,_,TVar y) when x=y -> ([y], TVar y)
  | (_,_,l,TVar y) when List.mem y l -> ([y],TVar y)
  | (_,_,_,TVar y) -> ([], TVar y)
  | (x,t',l,TFun(t1,t2)) -> (
    match (dotsubst (x, t') (l,t1), dotsubst (x, t') (l,t2)) with
    | ((l1,t1'), (l2,t2')) -> 
      let l1s = List.to_seq l1 |> IntSet.of_seq in
      let l2s = List.to_seq l2 |> IntSet.of_seq in
      let union = IntSet.union l1s l2s in 
      let unionl = IntSet.to_seq union |> List.of_seq in
      (unionl, TFun(t1',t2'))
  )
let dotsubsts = List.fold_right dotsubst;;

let dotsubsts_env (substs: tsubst list) (env: tenv) = NameMap.map (fun t -> dotsubsts substs t) env

let docsubst (s: tsubst) ((t1,t2): constr) =
  let ((_,t1'),(_,t2')) = (dotsubst s ([],t1), dotsubst s ([],t2)) in
  (t1',t2')

let is_unifying (s: tsubst) ((t1,t2): constr) =
  (dotsubst s ([],t1)) = (dotsubst s ([],t2))
;;

let rec toccurs (x: int) (t: typing) : bool = match t with
  | TVar y when x=y -> true
  | TVar _
  | TBool
  | TInt -> false
  | TFun(t1,t2) -> (toccurs x t1) || (toccurs x t2)
;;

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
;;

let instantiate max_tvar ((l,t): tscheme) : (typing * int) =
  let substs = List.mapi
  (fun i x -> (x, TVar (max_tvar+i+1))) l in
  let max' = max_tvar + (List.length l) in
  let (l,t) = dotsubsts substs ([],t) in
  assert(l = []); (t, max')

let rec vars_in_t = function
| TInt
| TBool -> IntSet.empty
| TVar x -> IntSet.add x (IntSet.empty)
| TFun(t1,t2) -> IntSet.union (vars_in_t t1) (vars_in_t t2)

let vars_in_env (env: tenv) : (IntSet.t) = 
let b = NameMap.bindings env in
let k = List.filter_map (fun (_,(_,k)) -> (match k with | TVar x -> Some x | _ -> None)) b in
List.to_seq k |> IntSet.of_seq

let generalize c1 env x t = 
match unify c1 with
| Ok(substl) ->
    let (_,u1) = dotsubsts substl ([],t) in
    let env1 = dotsubsts_env substl env in
    let vars_in_u1 = vars_in_t u1 in
    let vars_in_env1 = vars_in_env env1 in
    let diff = IntSet.diff vars_in_u1 vars_in_env1 in
    let diffl = IntSet.to_seq diff |> List.of_seq in
    Ok(tbind (NVar x) (diffl,u1) env1)
| Error(_) as err -> err

let rec getconstrs (env: tenv) (max_tvar: int) = function
  | ET(e) -> (match e with
    | CNum _ -> (TInt, max_tvar, empty)
    | CBool _ -> (TBool, max_tvar, empty)
    | EFun(x,e) -> 
      let fresh_i = max_tvar+1 in
      let fresh = TVar fresh_i in
      let newenv = tbind (NVar x) ([],fresh) env in
      let (t2, max, c) = getconstrs newenv fresh_i e in
      (TFun(fresh, t2), max, c)

    | EClosure(_,_,_) -> failwith "EClosure should be runtime only"
  )

  | ENT(e) -> (match e with
    | EName(x) -> (match tlookup x env with
      | Some(t) -> (match instantiate max_tvar t with 
        | (t, max') -> (t, max', empty)
      )
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
      let c = ConstrSet.add (t1, TFun(t2, fresh)) empty in
      let united = union_mul [c1;c2] c in
      (fresh, max, united)

    | ELetIn(x,e1,e2) -> ((* modified to allow recursion *)
      let fresh_i = 1+max_tvar in 
      let fresh = TVar fresh_i in
      let env' = tbind (NVar x) ([],fresh) env in
      let (t1,max,c1) = getconstrs env' fresh_i e1 in
      match generalize c1 env' x t1 with
      | Ok(gen_env) -> 
        let _ = tbind (NVar x) ([],t1) env in
        let (t2,max,c2) = getconstrs gen_env max e2 in
        let united = ConstrSet.union c1 c2 in
        (t2,max,united)
      | Error(_) -> failwith "constraints"
    )

    | ERet(_)
    | EThunk(_,_)
    | EUPrim(_,_)
    | EBPrim(_,_,_) -> failwith "Runtime only"
  )
;;

(*Input: types such as 'hello -> 'wow -> 'hello
  Output: 'a -> 'b -> 'a *)
let norm_vars t (start_from: int) (fresh: int) = 
  let rec helper (start_from: int) (next:int) m = (function
    | TInt -> (TInt, next, m)
    | TBool -> (TBool, next, m)
    | TVar x when x < start_from -> (TVar x, next, m)
    | TVar x -> (match IntMap.find_opt x m with
      | Some y -> (TVar y, next, m)
      | None -> (TVar next, next+1, IntMap.add x next m))
    | TFun(t1,t2) -> 
      let (t1', next', m') = helper start_from next m t1 in
      let (t2', next'', m'') = helper start_from next' m' t2 in
      (TFun(t1',t2'), next'', m'')
  ) 
  in let (t',_,_) = helper start_from fresh IntMap.empty t
  in t'

let norm_vars_sch ((l,t): tscheme) =
  let rec helper (fresh:int) = function
    | ([],t) -> ([],norm_vars t fresh fresh)
    | (v::l,t) -> (
      let (l',t') = dotsubst (v,TVar fresh) ((l,t)) in
      match helper (fresh+1) (l',t') with
      | (l'',t'') -> (fresh::l'',t'')
    )
in helper 0 (l,t)

let tinfer_expr (env: tenv) (e: expr) = 
  let (t,_,constrs) = getconstrs env (-1) e in
  match unify constrs with
  | Ok(substs) -> let (_,t) = dotsubsts substs ([],t) in
      Ok(norm_vars_sch (IntSet.to_seq (vars_in_t t) |> List.of_seq, t))
  | Error(_) as err -> err

let types_equal (t1: typing) (t2: typing) = (norm_vars t1 0 0) = (norm_vars t2 0 0)

type progCheckError = 
  | UnsatConstr of ide * (constr list)
  | DifferentType of ide * typing * typing

let rec get_typenv (l: (ide*expr*(typing option)) list) : (tenv,progCheckError) result =
  match l with
  | [] -> Ok static_tenv
  | (ide,e,Some(tsig))::t -> (match get_typenv t with
    | Ok(tenv) -> (match tinfer_expr tenv e with
      | Ok((_,t)) when types_equal tsig t -> Ok(tbind (NVar ide) ([],tsig) tenv)
      | Ok((_,t)) -> Error(DifferentType(ide, tsig, t))
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