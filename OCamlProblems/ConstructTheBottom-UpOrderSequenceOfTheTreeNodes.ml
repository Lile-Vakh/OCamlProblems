type 'a mult_tree = T of 'a * 'a mult_tree list;;

let bottom_up t =
  let rec aux (T (x, sub)) l =
    List.fold_right (fun t l -> aux t l) sub (x::l)
  in aux t [];;

(* Version #2 *)
let bottom_up t =
  let rec aux acc t =
    match t with
    |T (x, sub) -> List.fold_left (fun l tree -> aux (x::acc) tree) (x::acc) sub
  in aux [] t;;


