let n : int = 10 -- n-th power of 2 to compute

-- compute n^m
in let power : int -> int -> int = 
  fun n -> fun m -> if m = 0 then 1 else n*power n (m-1) 

-- compute 2^m
in let powersof2 = power 2

in powersof2 n