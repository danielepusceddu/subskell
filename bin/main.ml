open SubskellLib.Main;;
open SubskellLib.Ast;;
open SubskellLib.Typecheck;;

print_endline("\nTesting powers of 2...");
assert ("power :: int -> int -> int
power = let power = \\n -> \\m -> if m = 0 then 1 else n*power n (m-1) in power

powersof2 :: int -> int
powersof2 = power 2.
main = do powersof2 10"  |> parse |> (eval_prog (-1)) = Ok (ET (CNum 1024)));
print_endline("Test passed.");

print_endline("\nTesting Fibonacci...");
assert ("fib :: int -> int
fib = let fib = \\n -> if n = 0 then 0 else if n = 1 then 1 else  fib (n-1) + fib (n-2) in fib.
main = do fib 10" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 55)));
print_endline("Test passed.");

print_endline("\nTesting id...");
assert ("id = let id = \\x -> x in id.
main = do if id true then id 5 else id 9" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 5)));
print_endline("Test passed.");

print_endline("\nTesting eq...");
assert ("id = let id = \\x -> x in id.
main = do if (id true) = true && (id 5) = 5 then id 1337 else id 420" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 1337)));
print_endline("Test passed.");

let has_type (env: tenv) (x: ide) (t: tscheme) = match tlookup (NVar x) env with
| Some(t2) -> t=t2
| None -> false in

print_endline("\nTesting powers of 2 typecheck...");
assert (match ("power = let power = \\n -> \\m -> if m = 0 then 1 else n*power n (m-1) in power
powersof2 = power 2.
main = do powersof2 10" |> parse |> typecheck_prog) with
| Ok(maint, tenv) -> let has_type = has_type tenv in 
maint=([],TInt) && (has_type "power" ([],(TFun(TInt, TFun(TInt, TInt))))) && (has_type "powersof2" ([],(TFun(TInt, TInt))))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting id typecheck...");
assert (match ("id = let id = \\x -> x in id.
main = do if id true then id 5 else id 9" |> parse |> typecheck_prog) with
| Ok(maint, tenv) -> let has_type = has_type tenv in 
maint=([],TInt) && (has_type "id" ([0],(TFun(TVar 0, TVar 0))))
| _ -> false
);
print_endline("Test passed.");

print_endline("\nTesting eq typecheck...");
assert (match ("booleq = (=) true
inteq = (=) 1.
main = do (=)" |> parse |> typecheck_prog) with
| Ok(maint, tenv) -> let has_type = has_type tenv in 
maint=([0],TFun(TVar 0, TFun(TVar 0, TBool))) && (has_type "booleq" ([],(TFun(TBool, TBool) ))) && (has_type "inteq" ([],(TFun(TInt, TBool))))
| _ -> false
);
print_endline("Test passed.");
| _ -> false
);
print_endline("Test passed.");