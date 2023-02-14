open SubskellLib.Main;;

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s
;;

match Sys.argv with
| [| _;"annotate"; file |] -> print_endline("\nDo annotation " ^ file)

(* Run the source code from a file *)
| [| _;"run"; file |] -> 
  
  let source = read_file file in
  print_endline("\nSource code of " ^ file ^ ":\n\n"^source);
  let past = parse source in
  let result = eval_prog (-1) past in
  print_endline("\n\n"^(string_of_evalresult result))

| _ -> print_endline ("\nUsage: dune exec subskell [annotate|run] <file>" ^ (string_of_int (Array.length(Sys.argv))))