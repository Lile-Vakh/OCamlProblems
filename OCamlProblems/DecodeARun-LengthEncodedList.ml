type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let decode list =
  let rec add e n acc = match n with
    |0 -> acc
    |_ -> add e (n - 1) (e::acc) 
  in let rec concat list acc =
       match list with
       |[] -> acc
       |Many (a, b)::rest -> concat rest (acc @ add b a [])
       |One a :: rest -> concat rest (acc @ add a 1 [])
  in concat list [];;