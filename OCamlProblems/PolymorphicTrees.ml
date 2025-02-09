type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
                                 
let rec insert v compare = function 
  |Empty -> Node (v, Empty, Empty)
  |Node (x, l, r) -> let c = compare x v 
      in if c < 0 then Node (x, l, insert v compare r) 
      else if c > 0 then Node (x, insert v compare l, r) 
      else Node (x, l, r)
          
let rec string_of_tree v_to_string = function
  |Empty -> "Empty"
  |Node (v, l, r) -> "Node (" ^ (v_to_string v) ^ ", " ^
                     (string_of_tree v_to_string l) ^ ", " ^
                     (string_of_tree v_to_string r) ^ ")"
  
let inorder_list t =
  let rec imp tree acc =
    match t with
    |Empty -> acc
    |Node (v, l, r) -> imp l (v::imp r acc)
  in imp t []

(* Version #2 - not tail recursive *)
let rec inorder_list t =
  match t with
  |Empty -> []
  |Node (v, l, r) -> inorder_list l @ v::(inorder_list r)
  
(* Version #3 - not tail recursive *)              
let inorder_list t =
  let rec impl q t acc = match t with 
    |Node (x, l, r) ->  impl (Node (x, Empty, r)::q) l acc
    | Empty -> match q with 
      |[] -> acc | Node (x, _, r)::qs -> impl qs r (x::acc)
      | _ -> failwith "unreachable"
  in List.rev (impl [] t [])

let delete node tree =
  let rec min t =
    match t with
    |Empty -> Empty
    |Node(v, Empty, Empty) -> v
    |Node(v, l, r) -> min l 
  in let rec delete n tr =
       match tr with
       |Empty -> Empty
       |Node(v, l, r) -> if n < v then Node(v, delete n l, r)
           else if n > v then Node(v, l, delete n r)
           else match l, r with
             |Empty, Empty -> Empty
             |Empty, _ -> r
             |_, Empty -> l
             |_ -> Node(min r, l, delete (min r) r)
  in delete node tree;;
    
  

