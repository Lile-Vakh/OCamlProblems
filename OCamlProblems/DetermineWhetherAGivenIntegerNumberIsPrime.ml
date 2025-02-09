let is_prime n =
  let rec divide n k =
    if n <> k then if n mod k = 0 then false
      else divide n (k + 1) 
    else true 
  in let rec aux num k =
       match n with
       |0|1|2 -> true
       |_ -> if divide num k then true else false
  in aux n 2;;

