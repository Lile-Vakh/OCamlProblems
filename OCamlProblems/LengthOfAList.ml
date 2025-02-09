let length lst =
    let rec helper n = function
        |[] -> n
        |x::xs -> helper (n + 1) xs
    in helper 0 lst;;