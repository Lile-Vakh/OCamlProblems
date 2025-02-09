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

let goldbach n =
  let rec aux n1 n2 =
    if is_prime n1 && is_prime n2
    then n1, n2 else aux (n1 + 1) (n2 - 1)
  in aux 2 (n - 2);;

