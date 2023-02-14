let n : int = 10

in let power : int -> int -> int = 
  fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 

in let powersof2 = power 2

in powersof2 n