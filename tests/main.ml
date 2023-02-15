open SubskellLib.Main;;
open SubskellLib.Typescommon;;
open SubskellLib.Typesstatic;;
open SubskellLib.Typesruntime;;
open SubskellLib.Typecheck;;

(* Format: (test label, source code, eval result) *)
let eval_tests = [
  ("powers of 2",
   "let power : int -> int -> int = 
    fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 
    in let powersof2 = power 2 in
    powersof2 10", Ok (ET (CNum 1024)));
  ("Fibonacci",
   "let fib : int -> int = fun n -> 
    if n = 0 then 0 
    else if n = 1 then 1 
    else fib (n-1) + fib (n-2)
    in fib 10", Ok (ET (CNum 55)));
  ("id",
   "let id = fun x -> x
    in if id true then id 5 else id 9", Ok (ET (CNum 5)));
  ("eq polymorphism", 
   "let id = fun x -> x
    in let eq = (=)
    in if (id true) = true && (id 5) = 5 
      then id 1337 
      else id 420", Ok (ET (CNum 1337)));
]
;;

(* Format: (test label, source code, main type, list of names and their expected types) *)
let typecheck_tests = [
  ("powers of 2", 
   "let power = 
    fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 
    in let powersof2 = power 2 in
    powersof2 10", TInt, [("power", ([],TFun(TInt, TFun(TInt, TInt)))); ("powersof2", ([],TFun(TInt, TInt)))]);
  ("id",
   "let id = fun x -> x
    in if id true then id 5 else id 9", TInt, [("id", ([1],TFun(TVar 1, TVar 1)))]);
  ("eq",
   "let booleq = (=) true in
    let inteq = (=) 1
    in (=)", TFun(TVar 6, TFun (TVar 6, TBool)), [("booleq", ([],TFun(TBool, TBool))); ("inteq", ([],TFun(TInt, TBool)))])
]
;;

let rec find_1st_letin (e: aexpr) (x: ide) = match e with
   | ALetIn(y,_,_,_) as needle when x=y -> Some needle
   | ALetIn(_,_,e1,e2) -> (match find_1st_letin e1 x with
     | Some _ as n -> n
     | None -> find_1st_letin e2 x
  )
  | AIf(e1,e2,e3) -> (match find_1st_letin e1 x with
    | Some _ as n -> n
    | None -> (match find_1st_letin e2 x with
      | Some _ as n -> n
      | None -> find_1st_letin e3 x
    )
  )
  | AApp(e1,e2) -> (match find_1st_letin e1 x with
    | Some _ as n -> n
    | None -> find_1st_letin e2 x
  )
  | AFun(_,e) -> find_1st_letin e x
  | AName _
  | ANum _
  | ABool _ -> None
;;

let has_type e (x: ide) (t: tscheme) = match find_1st_letin e x with
   | Some ALetIn(_,t2,_,_) -> t=t2
   | _ -> failwith ("test has no letin with " ^ x)
;;


let%test _ =
  print_newline();
  print_endline ("*** Testing eval...");
  List.fold_left
  (fun b (label,source,evalres) ->
    print_string ("\n* Testing " ^ label ^ "...");
    let past = parse source
    in let b' = (eval_prog (-1) past) = evalres 
    in print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_evalresult evalres ^ "]"));
    b && b')
  true
  eval_tests


let%test _ =
  print_endline("\n\n\n*** Testing typecheck...");
  List.fold_left
  (fun b (label,source,maintype,subtests) ->
    let past = parse source in
    print_string ("\n* Testing " ^ label ^ "...");
    let b' = (match (tinfer_expr static_tenv past) with
      | Ok(maint, e') -> (let has_type' = has_type e' 
        in (maint = maintype) && (List.fold_left
          (fun b (id,tsch) -> b && (has_type' id tsch))
          true
          subtests))
      | _ -> false)

    in print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_type maintype ^ "]"));
    b && b'
  )
  true
  typecheck_tests;;


print_endline("\n\nTesting stricter type success...");
assert (match ("
let inteq : int -> int -> bool = (=)
in let booleq : bool -> bool -> bool = (=)
in let inteqtest : bool = inteq 5 5
in let booleqtest : bool = booleq true true
in inteqtest && booleqtest" |> parse |> (tinfer_expr static_tenv)) with
| Ok(maint, e') -> let has_type = has_type e' in 
maint=TBool && (has_type "inteq" ([],TFun(TInt, TFun(TInt, TBool)))) 
&& (has_type "booleq" ([],TFun(TBool, TFun(TBool, TBool))))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting stricter type failure...");
assert (match ("
let inteq : int -> int -> bool = (=)
in let wronghint : 'a. 'a -> 'a -> 'a = inteq
in (=)" |> parse |> (tinfer_expr static_tenv)) with
| Error(BadTypeHint("wronghint", ([0], TFun (TVar 0, TFun (TVar 0, TVar 0))), ([], TFun (TInt, TFun (TInt, TBool))))) -> true
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting stricter or equal...");
assert(
stricter_or_equal ([0;1], TFun(TVar 0, TVar 1)) ([0;1],TFun(TVar 0, TVar 1)) = true &&
stricter_or_equal ([], TFun(TInt, TBool)) ([0;1],TFun(TVar 0, TVar 1)) = true &&
stricter_or_equal ([2;5], TFun(TVar 5, TVar 2)) ([0;1],TFun(TVar 0, TVar 1)) &&
stricter_or_equal ([],TFun(TInt, TInt)) ([], TFun(TInt, TInt)) = true &&
stricter_or_equal ([0;1],TFun(TVar 0, TVar 1)) ([], TFun(TInt, TBool)) = false &&
stricter_or_equal ([],TFun(TInt, TInt)) ([], TFun(TInt, TBool)) = false
);
print_endline("Test passed.");