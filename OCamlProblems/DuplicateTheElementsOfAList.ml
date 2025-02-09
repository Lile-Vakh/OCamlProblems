let dublicate list =
  let rec aux list acc =
    match list with
    |[] -> acc
    |x::xs -> aux xs (x::x::acc)
  in List.rev (aux list []);;
