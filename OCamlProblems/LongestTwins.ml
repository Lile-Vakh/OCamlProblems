let lt_seq list = 
  let rec existing e = function
    |[] -> false
    |h::t -> if h = e then true else existing e t
  in let rec twins x counter = function
      |[] -> false
      |h1::t1 -> if h1 = x then 
            (if (counter + 1) = 2 then true 
             else twins x (counter + 1) t1)
          else twins x counter t1
  in let rec aux acc = function
      |[] -> acc
      |x::xs -> if existing x acc then aux acc xs
          else if twins x 0 list then aux (acc @ [x]) xs 
          else aux acc xs
  in aux [] list;; 
  