type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type rat = int * int (* num, denom *)
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr

let rec eval_expr = 
  function Const f -> f 
         |UnOp (Neg, e) -> let n,d = eval_expr e in -n, d
         | BinOp (op, e1, e2) ->
             let (n1,d1) = eval_expr e1 in
             let (n2,d2) = eval_expr e2 in
             match op with
             | Add -> (n1*d2+n2*d1,d1*d2)
             | Sub -> (n1*d2-n2*d1,d1*d2)
             | Mul -> (n1*n2,d1*d2)
             | Div -> (n1*d2,d1*n2)