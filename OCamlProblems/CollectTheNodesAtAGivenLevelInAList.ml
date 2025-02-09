let at_level t level =
  let rec aux t level acc =
    match t with
    |Empty -> acc
    |Node (x, l, r) -> if level = 1 then x::acc
        else aux l (level - 1) (aux r (level - 1) acc) 
  in aux t level [];;
    