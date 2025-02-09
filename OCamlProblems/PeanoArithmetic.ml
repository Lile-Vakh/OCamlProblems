type nat = Zero | Succ of nat

let rec int_to_nat n =
  if n <= 0 then Zero 
  else Succ (int_to_nat (n - 1));;
 
let rec nat_to_int n =
  match n with
  |Zero -> 0
  |Succ x -> 1 + nat_to_int x;; 

let rec add x y =
  match x with
  |Zero -> y
  |Succ a -> add a (Succ y);; 

let rec mul x y =
  match x with
  |Zero -> Zero
  |Succ a -> add y (mul a y);;

let rec pow x y =
  match y with
  |Zero -> Succ Zero
  |Succ a -> mul x (pow x a);;
  
let rec leq x y =
  match x, y with
  |Succ x', Succ y' -> leq x' y'
  |Zero, Succ y' -> true
  |Zero, Zero -> true
  |Succ x', Zero -> false;;

