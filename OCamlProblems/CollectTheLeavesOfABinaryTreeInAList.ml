type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let count_leaves tree =
  let rec aux t acc =
    match t with
    |Empty -> acc
    |Node (x, Empty, Empty) -> x::acc
    |Node (x, l, r) -> aux l (aux r acc)
  in aux tree [];;
    