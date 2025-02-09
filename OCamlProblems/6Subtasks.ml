let f1 acc (x, y) = acc @ [y, x];;

let f2 acc x = if (List.length acc) mod 2 = 0 
  then x::acc else acc @ [x];;
    
let f3 acc (k, v) = fun x -> if x = k 
  then v else acc x;;
      
let map f list =
  let rec aux f l acc =
    match l with
    |[] -> acc
    |x::xs -> aux f xs ((f x)::acc)
  in List.rev(aux f list []);; 

let replicate n x =
  let rec aux num acc =
    if n < 1 then acc
    else aux (num - 1) (x::acc)
  in List.rev(aux n []);; 

type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist);;

let rec map_over_custom_llist f list =
  fun () -> match list() with
    |NilC -> NilC
    |ConsC(a, t) -> ConsC(f a, map_over_custom_llist f t);;

let rec map_over_ocaml_llist f lst =
  lazy (
    match Lazy.force lst with
    |NilO -> NilO
    |ConsO (x, xs) -> ConsO (f x, map_over_ocaml_llist f xs)
  );;

let rec merge_custom_llists lst1 lst2 = 
  fun () ->
    match lst1(), lst2() with
    | NilC, _ -> lst2()
    | _, NilC -> lst1()
    | ConsC (x, xs), ConsC (y, ys) ->
        if x <= y 
        then ConsC (x, merge_custom_llists xs lst2)
        else ConsC (y, merge_custom_llists lst1 ys);;

let rec merge_ocaml_llists lst1 lst2 =
  lazy(
    match Lazy.force lst1, Lazy.force lst2 with
    |NilO, _ -> Lazy.force lst2
    |_, NilO -> Lazy.force lst1
    |ConsO (x, xs), ConsO (y, ys) -> 
        if x <= y 
        then ConsO(x, merge_ocaml_llists xs lst2)
        else ConsO(y, merge_ocaml_llists lst1 ys)
  );;

let rec drop_dupl_custom_llist l =
  fun () -> match l() with
    |NilC -> NilC
    |ConsC(x, xs) -> match xs() with
      |NilC -> ConsC(x, fun () -> NilC) 
      |ConsC(y, ys) -> if x = y 
          then drop_dupl_custom_llist xs ()
          else ConsC (x, drop_dupl_custom_llist xs);;

let rec drop_dupl_ocaml_llist l =
  lazy (
    match Lazy.force l with
    | NilO -> NilO
    | ConsO (x, xs) ->
        match Lazy.force xs with
        | NilO -> ConsO (x, lazy NilO)
        | ConsO (y, ys) ->
            if x = y then Lazy.force (drop_dupl_ocaml_llist xs)
            else ConsO (x, drop_dupl_ocaml_llist xs)
  );;

let rec divide_until_prime n p =
  if (n mod p) = 0 then divide_until_prime (n/p) p
  else n;;

let rec check_if_hamming n =
  match n with
  |1 -> true
  |x ->
      if x mod 2 = 0 then check_if_hamming (divide_until_prime x 2)
      else if x mod 3 = 0 then check_if_hamming (divide_until_prime x 3)
      else if x mod 5 = 0 then check_if_hamming (divide_until_prime x 5)
      else false;;

let rec hamming_custom () = 
  let rec helper n =
    if check_if_hamming n then ConsC(n, fun () -> helper (n+1))
    else helper (n+1)
  in
  helper 1;;

let rec hamming_ocaml =
  let rec helper n =
    if check_if_hamming n then ConsO(n, lazy (helper (n+1)))
    else helper (n+1)
  in
  lazy (helper 1);;
  
  
  
  
  