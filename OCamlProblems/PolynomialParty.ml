let eval_poly x coeffs =
  let rec impl value coeffs =
    match coeffs with 
    |[] -> value
    | c::cs -> impl ((value *. x) +. c) cs
  in impl 0.0 coeffs;;

let derive_poly coeffs =
  let rec impl = function 
    | [] | [_] -> ([], 0.)
    | c::cs -> let (new_coeffs, deg) = impl cs in
        (c *. (deg +. 1.))::new_coeffs, deg +. 1.
  in fst (impl coeffs);;