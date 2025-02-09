type 'a mult_tree = T of 'a * 'a mult_tree list;;

let rec count_nodes (T (x, sub)) =
  List.fold_left (fun acc t -> acc + count_nodes t) 1 sub;;