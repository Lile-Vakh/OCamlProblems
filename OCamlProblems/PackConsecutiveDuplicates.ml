let pack list =
  let rec adding x lst =
    match lst with 
    |[] -> [[x]] 
    |(h1::t1)::rest -> if h1 = x then (x::h1::t1)::rest 
        else [x]::(h1::t1)::rest 
    |[]::rest -> [x]::rest
  in let rec aux acc = function
      |[] -> acc
      |h2::t2 -> aux (adding h2 acc) t2
  in List.rev (aux [] list);;
     


  