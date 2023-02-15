-- With static scope, this program gives 10+1 = 11
-- With dynamic scope, this program would give 10+10 = 20

let n : int = 1
in let usen : int -> int = fun m -> n+m
in let useusen : int -> int = fun n -> usen n
in useusen 10