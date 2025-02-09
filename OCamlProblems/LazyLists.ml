type 'a llist = Cons of 'a * (unit -> 'a llist);;
                             
let rec lnat n = Cons (n, fun () -> lnat (n + 1));;
    
let lfib () =
  let rec aux a b = 
    Cons (a, fun () -> aux b (a + b))
  in aux 0 1;;

let rec ltake n (Cons (a, t)) =
  if n = 0 then []
  else a::ltake (n - 1) (t ());;

let rec lfilter p (Cons (h, t)) =
  if p h then Cons (h, fun () -> lfilter p (t ()))
  else lfilter p (t ());;