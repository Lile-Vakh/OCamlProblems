type 'a binary_tree = 
  | Empty 
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec string_of_tree t =
  match t with
  |Empty -> "Empty"
  |Node (v, l, r) -> let data = String.make 1 v in
      "Node (" ^ data ^ ", " ^ string_of_tree l ^ ", " ^
      string_of_tree r ^ ")";;

