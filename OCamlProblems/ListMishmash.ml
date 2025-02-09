   //Solution #1
let rec interleave3 lst1 lst2 lst3 = 
  match lst1, lst2, lst3 with
  |[],      [],      []      -> []
  |x1::xs1, x2::xs2, x3::xs3 -> x1::x2::x3::interleave3 xs1 xs2 xs3
  |[],      [],      x::xs   -> x::interleave3 [] [] xs
  |[],      x::xs,   []      -> x::interleave3 [] xs []
  |[],      x1::xs1, x2::xs2 -> x1::x2::interleave3 [] xs1 xs2 
  |x::xs,   [],      []      -> x::interleave3 xs [] []
  |x1::xs1, [],      x2::xs2 -> x1::interleave3 xs1 [] xs2
  |x1::xs1, x2::xs2, []      -> x1::x2::interleave3 xs1 xs2 [];;
      
   //Solution #2
let rec interleave3 lst1 lst2 lst3 = 
  let rec interleave2 lst1 lst2 =
    match lst1 with
    |[] -> lst2
    |x::xs -> x::interleave2 lst2 xs
  in match lst1 with
  |[] -> interleave2 lst2 lst3
  |x::xs -> x::interleave3 lst2 lst3 xs;;