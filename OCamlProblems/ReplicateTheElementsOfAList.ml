let replicate list n =
  let rec repl x n acc =
    if n = 0 then acc
    else repl x (n - 1) (x::acc) in
  let rec aux l acc =
    match l with
    |[] -> acc
    |x::xs -> aux xs (repl x n acc)
  in List.rev (aux list []);;

let replicate list n =
  let rec prepend n acc x =
    if n = 0 then acc 
    else prepend (n-1) (x :: acc) x 
  in List.fold_left (prepend n) [] (List.rev list);;

