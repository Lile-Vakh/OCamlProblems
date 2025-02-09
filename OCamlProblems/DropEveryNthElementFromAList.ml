let rec drop list n =
  match (list, n) with 
  |[], _ -> []
  |x::xs, 1 -> drop xs (n - 1)
  |x::xs, _ -> x::drop xs (n - 1);; 