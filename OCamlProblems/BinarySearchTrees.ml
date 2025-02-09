type 'a binary_tree = 
  | Empty 
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let construct list =
  let rec insert t x =
    match t with
    | Empty -> Node (x, Empty, Empty)
    | Node (v, l, r) -> 
        if v > x then Node (v, insert l x, r)
        else if v < x then Node (v, l, insert r x)
        else t
  in List.fold_left insert Empty list;;