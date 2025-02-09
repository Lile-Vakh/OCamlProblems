  //Solution #1
let palindrome lst =
    let rec aux acc = function
        |[] -> acc
        |x::xs -> aux (x :: acc) xs  
    in aux [] lst;;

let is_palindrome lst = 
    lst = palindrome lst;;

   //Solution #2
let is_palindrome lst =
  let rec aux acc = function 
    |[] -> acc
    |x::xs -> aux (x::acc) xs
  in let reversed = aux [] lst
    in lst = reversed;;