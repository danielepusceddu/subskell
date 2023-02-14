let id = fun x -> x
in let eq = (=)
in let eqtrue = (=) true
in let eqzero = (=) 0
in if eqtrue (id true) && eqzero (id 0)
   then id 1337 
   else id 420