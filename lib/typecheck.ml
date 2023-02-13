open Parsingast
open Help

(* AST in which type annotations are forced *)
type aexpr = 
  | ANum of int
  | ABool of bool
  | AFun of ide * aexpr
  | AApp of aexpr * aexpr
  | AIf of aexpr * aexpr * aexpr
  | AName of name
  | ALetIn of ide * typing * aexpr * aexpr

(* Types for type environment *)
module NameMap = Map.Make(struct type t = name let compare = compare end);;
type tenv = tscheme NameMap.t;;
let tbind : (name -> tscheme -> tenv -> tenv) = NameMap.add
let tlookup : (name -> tenv -> tscheme option) = NameMap.find_opt

(* This tenv contains the types of each predefined name. *)
let static_tenv : tenv = List.fold_left 
  (fun acc (key,v) -> NameMap.add key v acc) 

  NameMap.empty

  [(NBop(BOPlus), ([], TFun(TInt, TFun(TInt, TInt))));
   (NBop(BOMinus), ([], TFun(TInt, TFun(TInt, TInt))));
   (NBop(BOTimes), ([], TFun(TInt, TFun(TInt, TInt))));
   
   (NBop(BOAnd), ([], TFun(TBool, TFun(TBool, TBool))));
   (NBop(BOOr), ([], TFun(TBool, TFun(TBool, TBool))));

   (* BOEq is the interesting case: its captured type variable will
      be instantiated for each use. This allows us to use it for
      both integers and booleans. *)
   (NBop(BOEq), ([0], TFun(TVar 0, TFun(TVar 0, TBool))));
   (NBop(BOLeq), ([], TFun(TInt, TFun(TInt, TBool))));
   (NUop(UONot), ([], TFun(TBool, TBool)))
  ]

(* Type for type constraints *)
type constr = typing * typing;;
module ConstrSet = Set.Make(struct type t = constr let compare = compare end);;
let add_mul = List.fold_right ConstrSet.add;;
let empty = ConstrSet.empty;;
let union_mul = List.fold_right ConstrSet.union;;

(* Type substitutions *)
type tsubst = int * typing

(* Substitutes type variable 'x' for type 'newt' in typescheme l,t.
   Also cleans the list of captured variables l. *)
let rec dotsubst ((x,newt):tsubst) ((l,t):tscheme) = match (x, newt, l, t) with
  | (_,_,_,TInt) -> ([],TInt)
  | (_,_,_,TBool) -> ([],TBool)

  (* Do nothing when the variable we want to substitute is captured. *)
  | (x,t',l,TVar y) when x=y && (not (List.mem x l)) -> ([],t')

  (* When it is not captured, substitute it. *)
  | (x,_,_,TVar y) when x=y -> ([y], TVar y)

  (* When we encounter a different variable, see if we should clean the list. *)
  | (_,_,l,TVar y) when List.mem y l -> ([y],TVar y)
  | (_,_,_,TVar y) -> ([], TVar y)

  (* For functions, do the substitution in both types,
     then perform a union on their captured variables. 
     Might want to use sets to begin with? *)
  | (x,t',l,TFun(t1,t2)) -> 
    let (l1,t1') = dotsubst (x, t') (l,t1) in
    let (l2,t2') = dotsubst (x, t') (l,t2) in
    let l1s = List.to_seq l1 |> IntSet.of_seq in
    let l2s = List.to_seq l2 |> IntSet.of_seq in
    let union = IntSet.union l1s l2s in 
    let unionl = IntSet.to_seq union |> List.of_seq in
    (unionl, TFun(t1',t2'))
;;

(* Do multiple substitutions to a type scheme. Starts from the last. *)
let dotsubsts = List.fold_right dotsubst;;

(* Do multiple substitutions to each type scheme in the tenv. *)
let dotsubsts_env (substs: tsubst list) (env: tenv) = NameMap.map (fun t -> dotsubsts substs t) env

(* Perform a substitution on both sides of a constraint. *)
let docsubst (s: tsubst) ((t1,t2): constr) =
  let ((_,t1'),(_,t2')) = (dotsubst s ([],t1), dotsubst s ([],t2)) in
  (t1',t2')

(* True iff TVar x occurs in type t *)
let rec toccurs (x: int) (t: typing) : bool = match t with
  | TVar y when x=y -> true
  | TVar _
  | TBool
  | TInt -> false
  | TFun(t1,t2) -> (toccurs x t1) || (toccurs x t2)
;;

(* Attempt finding a series of substitutions to solve a set of constraints. *)
let unify (constrset: ConstrSet.t) : (tsubst list, constr list) result = 
  let rec helper = function
    (* No constraints to solve; no substitutions needed. *)
    | [] -> Ok([])

    (* Discard tautologies. *)
    | (TInt, TInt)::t
    | (TBool, TBool)::t -> helper t
    | (TVar x, TVar y)::t when x=y -> helper t

    (* When the constraint is 'x = t, check that 'x does not occur in t.
       If it doesn't, try to solve every other constraint after 
       replacing 'x with t. *)
    | (TVar x, ty)::t when not (toccurs x ty) -> (
      let t' = List.map (docsubst (x,ty)) t in 
      match helper t' with
      | Ok(substs) -> Ok((x,ty)::substs)
      | Error(_) as err -> err
    )
    | (ty, TVar x)::t when not (toccurs x ty) -> ( (* same as above *)
      let t' = List.map (docsubst (x,ty)) t in 
      match helper t' with
      | Ok(substs) -> Ok((x,ty)::substs)
      | Error(_) as err -> err
    )

    (* Split function equalities in two constraints:
       the first to check if the input types are equal,
       the second to check if the output types are equal. *)
    | (TFun(i1,o1),TFun(i2,o2))::t -> helper((i1,i2)::(o1,o2)::t)

    (* Every other case results in an unsatisfiable set of constraints. *)
    | l -> Error(l)
  in helper (ConstrSet.to_seq constrset |> List.of_seq)
;;

(* Take a type scheme such as 'a 'b. 'a -> 'b and instantiate it to a type
   such as 'c -> 'd. *)
let instantiate max_tvar ((l,t): tscheme) : (typing * int) =
  (* The i-th variable in l gets substituted with the i-th fresh variable. *)
  let substs = List.mapi
  (fun i x -> (x, TVar (max_tvar+i+1))) l in
  let max' = max_tvar + (List.length l) in
  let (l,t) = dotsubsts substs ([],t) in
  assert(l = []); (t, max')

(* The set of type variables that occur in a type. *)
let rec vars_in_t = function
| TInt
| TBool -> IntSet.empty
| TVar x -> IntSet.add x (IntSet.empty)
| TFun(t1,t2) -> IntSet.union (vars_in_t t1) (vars_in_t t2)

(* The set of type variables that occur in a type environment. *)
let vars_in_env (env: tenv) : (IntSet.t) = 
let b = NameMap.bindings env in
let k = List.filter_map (fun (_,(_,k)) -> (match k with | TVar x -> Some x | _ -> None)) b in
List.to_seq k |> IntSet.of_seq

(* Finish type inference of t in a tenv env with a set of constraints c1.
   Then, turn the obtained type u1 into a type scheme.
   Only the type variables that occur exclusively in u1 will be generalized:
   the others might depend on future constraints. *)
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

(* Changes all n variables in a type, using n fresh variables. *)
let rec refresh (t: typing) (max_tvar: int) = match t with
  | TInt -> (TInt, max_tvar)
  | TBool -> (TBool, max_tvar)
  | TVar x -> (TVar (x+1), max_tvar+1)
  | TFun(t1,t2) -> let (t1',max') = refresh t1 max_tvar in 
                   let (t2',max') = refresh t2 max' in 
                   (TFun(t1',t2'), max')

(* Adds a constraint for the inferred type to be equal to the hint type, if any.
   This has issues: for example, the programmer hints t1 = 'a -> 'a
   and we infer t2 = int -> int. The resulting constraint will be satisfied,
   even if the programmer wants a less strict type.
   Solution(?): check that the substitutions do not change t1. *)
let hint2constr (hint: typing option) (inferred: typing) (max_tvar: int) : (ConstrSet.t * int)= match hint with
  | None -> (ConstrSet.empty, max_tvar)
  | Some(t) -> let (t',max') = refresh t max_tvar in
               (ConstrSet.add (t',inferred) ConstrSet.empty,max')

type infer_error = NameWithoutType of name | UnsatConstr of pexpr * (constr list)

(* We infer the type of an expression in a certain type environment.
   The result is a type, a set of constraints, and an annotated parse tree.
   We also keep track of the next fresh type variable we may use. *)
let rec getconstrs (env: tenv) (max_tvar: int) = function
  (* These expressions have primitive types:
     no constraints are generated, 
     and the types we infer contain no type variables. *)
  | PNum n -> Ok(TInt, ANum n, max_tvar, empty)
  | PBool b -> Ok(TBool, ABool b, max_tvar, empty)

  (* Function: generate a type variable for the function's argument,
     insert it into the type environment, and infer the type of 
     the function's body along with its constraints. No constraints are added. *)
  | PFun(x,e) -> (
    let fresh_i = max_tvar+1 in
    let fresh = TVar fresh_i in
    let newenv = tbind (NVar x) ([],fresh) env in
    match getconstrs newenv fresh_i e with 
    | Error(_) as e -> e
    | Ok(t2, e', max, c) -> Ok(TFun(fresh, t2), AFun(x,e'), max, c)
    )

  (* Name: instantiate the type scheme associated with the name in our tenv.
     Different instantiations may have different types. 
     This allows type polymorphism for the equality function and similar.
     No constraints are added. *)
  | PName(x) -> (match tlookup x env with
    | Some(t) -> (match instantiate max_tvar t with 
      | (t, max') -> Ok((t, AName(x), max', empty))
    )
    | None -> Error(NameWithoutType x)
  )

  (* If: generate a fresh type variable t representing the type of the if expression.
     Infer the types of e1, e2, e3, and get their constraints.
     Then add the constraints t1=bool, t=e2, t=e3. *)
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

  (* App: generate a fresh type variable t for the result of this expression.
     Infer t1 and t2, along with their constraints. 
     Add the constraint t1 = TFun(t2, t) *)
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

  (* let-in: create a fresh variable for the type of x and add it to the environment.
     Then, infer the type of x with its constraints.
     Add a constraint that the type of x is the same as its type hint.
     Finish inference of x and get its final type scheme, then bind it in our tenv in place of the previously generated type variable.
     Finally, infer the type of e2 and add its constraints. *)
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

(* Wrapper for getconstrs that starts at tvar 0,
   solves the constraints and applies the resulting substitution to
   get the final type. *)
let tinfer_expr (env: tenv) (e: pexpr) = 
  match getconstrs env (-1) e with
  | Error(_) as err -> err
  | Ok(t,e',_,constrs) -> (
    match unify constrs with
    | Ok(substs) -> 
        let (_,t) = dotsubsts substs ([],t) in
        Ok(t, e')
    | Error(err) -> Error(UnsatConstr(e, err))
  )