let encode list = 
  let rec aux count acc = function
    |[] -> []
    |[x] -> (count, x)::acc
    |a::(b::_ as t) -> if a = b then aux (count + 1) acc t
        else aux 1 ((count, a)::acc) t 
  in List.rev (aux 1 [] list);;
     


  