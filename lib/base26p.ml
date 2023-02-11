(* base10p: base10 but 00 is 10, 90 is 100, ...
   base26p: aa is the succ of z, not ba.
   idk if this actually exists *)

let char_to_num c = (Char.code c) - (Char.code 'a');;

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq
                          
let charl_to_string l = String.of_seq (List.to_seq l)

let int_of_base26p s =
  let rec helper = function
    | [] -> failwith "Empty string"
    | h::[] -> (char_to_num h,26) 
    | h::t -> let (n, mul) = helper t in (n+mul*(1+char_to_num h), mul*26)
  in let (n,_) = helper (string_to_char_list s) 
  in n;; 

let rec base26p_of_int = function
  | i when i >= 0 && i <= 25 -> Char.escaped (Char.chr (i + (Char.code 'a')))
  | i -> let (d,r) = (i/26, i mod 26) in (base26p_of_int (d-1) ^ (base26p_of_int r)) 
