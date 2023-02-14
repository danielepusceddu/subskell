open SubskellLib.Main;;
open SubskellLib.Runtimeast;;
open SubskellLib.Parsingast;;
open SubskellLib.Typecheck;;

print_endline("\nTesting powers of 2...");
assert ("
let power : int -> int -> int = 
  fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 
in let powersof2 = power 2 in
powersof2 10"  |> parse |> (eval_prog (-1)) = Ok (ET (CNum 1024)));
print_endline("Test passed.");

print_endline("\nTesting Fibonacci...");
assert ("
let fib : int -> int = fun n -> 
  if n = 0 then 0 
  else if n = 1 then 1 
  else fib (n-1) + fib (n-2)
in fib 10" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 55)));
print_endline("Test passed.");

print_endline("\nTesting id...");
assert ("
let id = fun x -> x
in if id true then id 5 else id 9" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 5)));
print_endline("Test passed.");

print_endline("\nTesting eq polymorphism...");
assert ("
let id = fun x -> x
in let eq = (=)
in if (id true) = true && (id 5) = 5 
   then id 1337 
   else id 420" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 1337)));
print_endline("Test passed.");

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
  in
let has_type e (x: ide) (t: tscheme) = match find_1st_letin e x with
   | Some ALetIn(_,t2,_,_) -> t=t2
   | _ -> failwith ("test has no letin with " ^ x)
in

print_endline("\nTesting powers of 2 typecheck...");
assert (match ("
let power = 
  fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 
in let powersof2 = power 2 in
powersof2 10" |> parse |> (tinfer_expr static_tenv)) with
| Ok(maint, e') -> let has_type = has_type e' in 
maint=TInt && (has_type "power" ([],TFun(TInt, TFun(TInt, TInt)))) && (has_type "powersof2" ([],TFun(TInt, TInt)))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting id typecheck...");
assert (match ("
let id = fun x -> x
in if id true then id 5 else id 9" |> parse |> (tinfer_expr static_tenv)) with
| Ok(maint, e') -> let has_type = has_type e' in 
maint=TInt && (has_type "id" ([1],TFun(TVar 1, TVar 1)))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting eq typecheck...");
assert (match ("
let booleq = (=) true in
let inteq = (=) 1
in (=)" |> parse |> (tinfer_expr static_tenv)) with
| Ok(maint, e') -> let has_type = has_type e' in 
maint=(TFun(TVar 6, TFun (TVar 6, TBool))) && (has_type "booleq" ([],TFun(TBool, TBool)))  && (has_type "inteq" ([],TFun(TInt, TBool)))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting stricter type success...");
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