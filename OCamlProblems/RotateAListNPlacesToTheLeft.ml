let rotate list k =
  let rec aux l k =
    match l with
    |[] -> []
    |x::xs -> if k < 0 then aux l (List.length l + k)
        else if k = 1 then (xs @ [x])
        else aux (xs @ [x]) (k - 1)
  in aux list k;;
        

  
