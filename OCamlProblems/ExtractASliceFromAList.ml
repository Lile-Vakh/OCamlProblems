let slice list n1 n2 =
  let rec aux l a b acc =
    match (a, b, l) with
    |(_, _, []) -> acc
    |(z, c, x::xs) -> if (z <= 0 && c >= 0)
        then aux xs (a - 1) (b - 1) (acc @ [x]) 
        else aux xs (a - 1) (b - 1) acc
  in aux list n1 n2 [];;

  
