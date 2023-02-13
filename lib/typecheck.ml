open Parsingast
open Help

type aexpr = 
  | ANum of int
  | ABool of bool
  | AFun of ide * aexpr
  | AApp of aexpr * aexpr
  | AIf of aexpr * aexpr * aexpr
  | AName of name
  | ALetIn of ide * typing * aexpr * aexpr

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
   (NBop(BOEq), ([0], TFun(TVar 0, TFun(TVar 0, TBool))));
   
   (NBop(BOLeq), ([], TFun(TInt, TFun(TInt, TBool))));
   
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

let generalize_all t = (vars_in_t t, t);;

let generalize_nobind c1 env t = match unify c1 with
  | Ok(substl) ->
      let (_,u1) = dotsubsts substl ([],t) in
      let env1 = dotsubsts_env substl env in
      let vars_in_u1 = vars_in_t u1 in
      let vars_in_env1 = vars_in_env env1 in
      let diff = IntSet.diff vars_in_u1 vars_in_env1 in
      let diffl = IntSet.to_seq diff |> List.of_seq in
      Ok((diffl,u1), env1)
  | Error(_) as err -> err
;;

let generalize c1 env x t = match unify c1 with
  | Ok(substl) ->
      let (_,u1) = dotsubsts substl ([],t) in
      let env1 = dotsubsts_env substl env in
      let vars_in_u1 = vars_in_t u1 in
      let vars_in_env1 = vars_in_env env1 in
      let diff = IntSet.diff vars_in_u1 vars_in_env1 in
      let diffl = IntSet.to_seq diff |> List.of_seq in
      Ok(tbind (NVar x) (diffl,u1) env1)
  | Error(_) as err -> err

(* changes all n variables in a type, using n fresh variables *)
let rec refresh (t: typing) (max_tvar: int) = match t with
  | TInt -> (TInt, max_tvar)
  | TBool -> (TBool, max_tvar)
  | TVar x -> (TVar (x+1), max_tvar+1)
  | TFun(t1,t2) -> let (t1',max') = refresh t1 max_tvar in 
                   let (t2',max') = refresh t2 max' in 
                   (TFun(t1',t2'), max')

let hint2constr (hint: typing option) (inferred: typing) (max_tvar: int) : (ConstrSet.t * int)= match hint with
  | None -> (ConstrSet.empty, max_tvar)
  | Some(t) -> let (t',max') = refresh t max_tvar in
               (ConstrSet.add (t',inferred) ConstrSet.empty,max')

type infer_error = NameWithoutType of name | UnsatConstr of pexpr * (constr list)
let rec getconstrs (env: tenv) (max_tvar: int) = function
  | PNum n -> Ok(TInt, ANum n, max_tvar, empty)
  | PBool b -> Ok(TBool, ABool b, max_tvar, empty)
  | PFun(x,e) -> (
    let fresh_i = max_tvar+1 in
    let fresh = TVar fresh_i in
    let newenv = tbind (NVar x) ([],fresh) env in
    match getconstrs newenv fresh_i e with 
    | Error(_) as e -> e
    | Ok(t2, e', max, c) -> Ok(TFun(fresh, t2), AFun(x,e'), max, c)
    )
  | PName(x) -> (match tlookup x env with
    | Some(t) -> (match instantiate max_tvar t with 
      | (t, max') -> Ok((t, AName(x), max', empty))
    )
    | None -> Error(NameWithoutType x)
  )

  | PIf(e1, e2, e3) -> (
    let fresh_i = 1+max_tvar in
    let fresh = TVar fresh_i in
    match getconstrs env fresh_i e1 with
    | Error(_) as e -> e
    | Ok(t1,e1',max,c1) -> (match getconstrs env max e2 with
      | Error(_) as e -> e
      | Ok(t2,e2',max,c2) -> (match getconstrs env max e3 with
        | Error(_) as e -> e
        | Ok (t3,e3',max,c3) -> 
          let c = add_mul [(t1, TBool); (fresh, t2); (fresh, t3)] empty in 
          let united = union_mul [c1; c2; c3] c in
          Ok(fresh, AIf(e1',e2',e3'), max, united)
      )
    )
  )

  | PApp(e1, e2) -> (
    let fresh_i = 1+max_tvar in 
    let fresh = TVar fresh_i in
    match getconstrs env fresh_i e1 with
    | Error(_) as e -> e
    | Ok(t1,e1',max,c1) -> (match getconstrs env max e2 with
      | Error(_) as e -> e
      | Ok(t2,e2',max,c2) -> 
        let c = ConstrSet.add (t1, TFun(t2, fresh)) empty in
        let united = union_mul [c1;c2] c in
        Ok(fresh, AApp(e1',e2'), max, united)
    )
  )

  | PLetIn(x,hint,e1,e2) as p -> ((* modified to allow recursion *)
    let fresh_i = 1+max_tvar in 
    let fresh = TVar fresh_i in
    let env' = tbind (NVar x) ([],fresh) env in
    match getconstrs env' fresh_i e1 with
    | Error(_) as err -> err
    | Ok(t1,e1',max,c1) -> (
      let (chint,max) = hint2constr hint t1 max in
      let c1 = ConstrSet.union c1 chint in
      match generalize_nobind c1 env' t1 with
      | Error(err) -> Error(UnsatConstr(p, err))
      | Ok((diffl,u1), env1) -> (
        let gen_env = tbind (NVar x) (diffl,u1) env1 in
        match getconstrs gen_env max e2 with
        | Error(_) as err -> err
        | Ok(t2,e2',max,c2) -> 
          let united = ConstrSet.union c1 c2 in
          let (instantiated,_) = instantiate 0 (diffl,u1) in
          Ok(t2,ALetIn(x,instantiated, e1', e2'), max,united)
      )
    )
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
  in let (t',n,_) = helper start_from fresh IntMap.empty t
  in (t',n)

let norm_annotations e = 
  let rec helper fresh = function
  | ANum n -> (ANum n, fresh)
  | ABool b -> (ABool b, fresh)
  | AName x -> (AName x, fresh)

  | AFun(x, e) -> 
    let (e',fresh) = helper fresh e in
    (AFun(x,e'), fresh)

  | AApp(e1,e2) ->
    let (e1',fresh) = helper fresh e1 in
    let (e2',fresh) = helper fresh e2 in
    (AApp(e1',e2'), fresh)

  | AIf(e1,e2,e3) ->
    let (e1',fresh) = helper fresh e1 in
    let (e2',fresh) = helper fresh e2 in
    let (e3',fresh) = helper fresh e3 in
    (AIf(e1',e2',e3'), fresh)
  
  | ALetIn(x,t,e1,e2) ->
    let (t', fresh) = norm_vars t 0 fresh in
    let (e1',fresh) = helper fresh e1 in
    let (e2',fresh) = helper fresh e2 in
    (ALetIn(x,t',e1',e2'), fresh)
  in helper 0 e
;;

let norm_vars_sch ((l,t): tscheme) =
  let rec helper (fresh:int) = function
    | ([],t) -> ([],norm_vars t fresh fresh)
    | (v::l,t) -> (
      let (l',t') = dotsubst (v,TVar fresh) ((l,t)) in
      match helper (fresh+1) (l',t') with
      | (l'',t'') -> (fresh::l'',t'')
    )
in helper 0 (l,t)

let tinfer_expr (env: tenv) (e: pexpr) = 
  match getconstrs env (-1) e with
  | Error(_) as err -> err
  | Ok(t,e',_,constrs) -> (
    match unify constrs with
    | Ok(substs) -> 
        let (_,t) = dotsubsts substs ([],t) in
        Ok(norm_vars_sch (IntSet.to_seq (vars_in_t t) |> List.of_seq, t), norm_annotations e')
    | Error(err) -> Error(UnsatConstr(e, err))
  )

let types_equal (t1: typing) (t2: typing) = (norm_vars t1 0 0) = (norm_vars t2 0 0)
