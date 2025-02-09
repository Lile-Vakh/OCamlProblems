let insert e k list =
  let rec aux k l =
    match l with
    |[] -> []
    |x::xs -> if k = 1 then x::e::xs
        else x::(aux (k - 1) xs)
  in aux k list;;
        

  
