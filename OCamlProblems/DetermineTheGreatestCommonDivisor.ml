let gcd n1 n2 =
  let rec aux n k =
    if n > k then if n1 mod k = 0 && n2 mod k = 0 
      then k else aux n (k - 1)
    else aux k n
  in aux n1 n2;;