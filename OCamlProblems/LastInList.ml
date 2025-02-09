   //Solution #1
let rec last = function
[] -> None
|[t] -> Some t
|_::t -> last t;;

   //Solution #2
let rec last l = 
match l with
|[] -> raise Not_found
|[x] -> x
|x::xs -> last xs;;