type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode list =
  let link x count = if count = 0 then One x 
    else Many (count + 1, x) 
  in let rec aux count acc = function
      |[] -> []
      |[x] -> acc @ [(link x count)]
      |x::(y::_ as t) -> if x = y then aux (count + 1) acc t
          else aux 0 (acc @ [(link x count)]) t
  in aux 0 [] list;;
