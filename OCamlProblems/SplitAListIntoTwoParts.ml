let split list n =
  let rec aux i acc = function
    |[] -> acc, []
    |x::xs -> if i = 1 then ((acc @ [x]), xs)
        else aux (i - 1) (acc @ [x]) xs
  in aux n [] list;;

