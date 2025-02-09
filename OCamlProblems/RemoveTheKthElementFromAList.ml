let rec remove list k =
  match (list, k) with
  |([], _) -> []
  |(x::xs, n) -> if n = 0 then xs
      else x::remove xs (k - 1);;
        

  
