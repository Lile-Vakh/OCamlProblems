let compress list = 
  let rec origin x lst = 
    match lst with
    |[] -> true
    |h::t -> if (h = x) then false 
        else origin x t in 
  let rec aux acc = function
    |[] -> acc
    |x::xs -> if (origin x acc) then aux (acc @ [x]) xs
        else aux acc xs
  in aux [] list;; 


  