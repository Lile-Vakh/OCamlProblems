type 'a mult_tree = T of 'a * 'a mult_tree list;;

let ipl t =
  let rec ipl_sub len (T (x, sub)) =
    List.fold_left (fun acc t -> acc + ipl_sub (len + 1) t) len sub
  in ipl_sub 0 t;;
