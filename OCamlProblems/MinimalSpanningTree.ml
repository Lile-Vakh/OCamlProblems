let mst list =
  let rec find_min_edge lst min =
    match lst with
    | [] -> min
    | (z, f, y) :: rest -> 
        match min with
        | None -> find_min_edge rest (Some(z, f, y))
        | Some(z1, x, y1) -> if x > f then find_min_edge rest min
            else find_min_edge rest (Some(z, f, y))
  in find_min_edge list None