   //Solution #1
let rec rev = function
    |[] -> []
    |x::xs -> rev xs @ [x];;

   //Solution #2
let rev lst = 
    let rec aux acc = function
        |[] -> acc
        |x::xs -> aux (x::acc) xs
    in aux [] lst;;