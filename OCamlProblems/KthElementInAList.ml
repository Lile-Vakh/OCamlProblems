let rec nth k = function
    [] -> None
    |x::xs -> if (k = 1) then Some x else nth (k - 1) xs;;