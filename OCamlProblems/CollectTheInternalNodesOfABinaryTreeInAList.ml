let internals tree =
  let rec aux acc = function
    |Empty -> acc
    |Node (x, Empty, Empty) -> acc
    |Node (x, l, r) -> aux (x::aux acc r) l
  in aux [] tree;;
    