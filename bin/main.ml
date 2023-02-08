open SubskellLib.Main;;
open SubskellLib.Ast;;


print_endline("\nTesting powers of 2...");
assert ("power :: int -> int -> int
power = \\n -> \\m -> if m .= 0 then 1 else n*power n (m-1)

powersof2 :: int -> int
powersof2 = power 2.
main = do powersof2 10"  |> parse |> (eval_prog (-1)) = Ok (ET (CNum 1024)));
print_endline("Test passed.");

print_endline("\nTesting Fibonacci...");
assert ("fib :: int -> int
fib = \\n -> if n .= 0 then 0 else if n .= 1 then 1 else  fib (n-1) + fib (n-2).
main = do fib 10" |> parse |> (eval_prog (-1)) = Ok (ET (CNum 55)));
print_endline("Test passed.");