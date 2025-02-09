let fac n = 
  let rec aux num acc =
    if num < 2 then acc
    else aux (num - 1) (acc * num)
  in aux n 1;;

let remove a l =
  let rec aux a list acc =
    match list with
    |[] -> acc
    |x::xs -> if x = a then aux a xs acc
        else aux a xs (acc @ [x])
  in aux a l [];; 
                                     
let partition f l =
  let rec aux f list acc1 acc2 =
    match list with
    |[] -> acc1, acc2
    |x::xs -> if f x then aux f xs (acc1 @ [x]) acc2
        else aux f xs acc1 (acc2 @ [x])
  in aux f l [] [];;
      