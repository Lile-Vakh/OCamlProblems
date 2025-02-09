(*int foo(int x, int y, bool b) {
if(x > y) {
int t = x;
x = y;
y = t;
}
while(x < y) {
  if(b) {
     ++x;
  } else {
     --y;
  }
  b = !b;
}
return x;
}
*)

//from Java to OCaml
let foo x y b =
  let x, y = if x > y then y, x else x, y in
  let rec loop x y b =
    if x >= y then x 
    else if b then loop (x + 1) y (not b)
    else loop x (y - 1) (not b)
  in loop x y b;;
  