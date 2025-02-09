let f1 l = 
  let rec aux = function
    |[] -> 0
    |x::xs -> 1 + aux xs
  in aux l;; 

let f2 l =
  let rec copying_list acc l =
    match l with
    |[] -> acc
    |x::xs -> copying_list (acc @ [x]) xs
  in let rec aux l len acc = 
       match l with
       |[] -> acc
       |lst :: rest -> if (f1 lst > len) 
           then aux rest (f1 lst) (copying_list [] lst) 
           else aux rest len acc
  in aux l 0 [];;

let rec f3 list = match list with
  |[] -> []
  |(a, b) :: rest -> (b, a)::(f3 rest);;

(* Version #2 *)
let f3 list = List.map (fun (a, b) -> (b, a)) list;; 

let f4 list = 
  let with_out_first = function
    |[] -> []
    |x::xs -> xs
  in let rec every_first = function
      |[] -> []
      |x::y::z -> x::(every_first z) 
      |[x] -> [x]
  in (every_first (List.rev list)) @ if (List.length list mod 2 = 1) 
     then every_first (with_out_first list)
     else every_first list;;

(* Version #3 *)
let f4 list = if List.length list mod 2 = 0
  then List.fold_left (fun acc x -> if List.length acc  mod 2 = 0 
                        then acc @ [x] else x::acc) [] list
  else List.fold_left (fun acc x -> if List.length acc mod 2 = 0
                        then x::acc else acc @ [x]) [] list;; 
    
  
  