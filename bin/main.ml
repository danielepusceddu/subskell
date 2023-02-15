open SubskellLib.Main;;
open SubskellLib.Typecheck;;
open SubskellLib.Typesstatic;;

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s
;;

match Sys.argv with
(* Infer the type and annotate the source code from a file *)
| [| _;"annotate"; file |] ->
  let source = read_file file in 
  let past = parse source in
  (match tinfer_expr static_tenv past with
  | Ok(t,ae) -> print_endline("\nAnnotated source code of " ^ file ^ ":\n\n" ^ (string_of_aexpr ae) ^ "\n\nInferred type: " ^ (string_of_type t))
  | Error err -> print_endline("\n" ^ (string_of_infer_error err)))

(* Run the source code from a file *)
| [| _;"run"; file |] -> 
  
  let source = read_file file in
  print_endline("\nSource code of " ^ file ^ ":\n\n"^source);
  let past = parse source in
  let result = eval_prog (-1) past in
  print_endline("\n\n"^(string_of_evalresult result))

| _ -> print_endline ("\nUsage: dune exec subskell [annotate|run] <file>" ^ (string_of_int (Array.length(Sys.argv))))