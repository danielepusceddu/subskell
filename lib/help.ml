module MapString = Map.Make(String);;

(* function to check for duplicates in a list of strings using a Map *)
let has_dups lst =
  (* create a map from the list with each element as the key and a unit value *)
  let map = List.fold_left (fun m s -> MapString.add s () m)MapString. empty lst in
  (* return true if the size of the map is less than the length of the list *)
  List.length lst > MapString.cardinal map