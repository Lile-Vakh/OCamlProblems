let is_empty list =
  match List.length list with
  |0 -> true
  |_ -> false;;

let rec get k l =
  match l with
  |[] -> None
  |(k', g')::rest -> if k = k' then Some g' 
      else get k rest;;

let rec put k v l = 
  match l with
  |[] -> [(k, v)]
  |(k', v')::rest -> if k = k' then (k, v)::rest
      else (k', v')::(put k v rest);;

let rec contains_key k l =
  match l with
  |[] -> false
  |(k', v')::rest -> if k' = k then true 
      else contains_key k rest;;

let rec remove k l =
  match l with
  |[] -> []
  |(k', v')::rest -> if k = k' then remove k rest
      else (k', v')::(remove k rest);;

let rec keys l =
  match l with
  |[] -> []
  |(k, v)::rest -> k::(keys rest);; 

let rec values l = 
  match l with
  |[] -> []
  |(k, v)::rest -> v::(values rest);;




    
  
    

