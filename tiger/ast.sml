(* The abstract syntax tree for expression *)
structure Ast = struct


datatype Expr  = Const of int
               | Op    of Expr * BinOp * Expr
               | Variable of string
and BinOp = Plus
               | Minus
               | Mul
               | Div

datatype Stmt = Println of Expr
              | Assign of string*Expr

               

type mpp = real AtomMap.map 
     


exception DivisionByZero
fun binOpDenote Plus  x y = x + y
  | binOpDenote Minus x y = x - y
  | binOpDenote Mul   x y = x * y
  | binOpDenote Div   x y = if(y=0) then raise DivisionByZero else  x div y;

fun exprDenote (Const x)       = x
  | exprDenote (Op (x,oper,y)) = binOpDenote oper (exprDenote x) (exprDenote y)

(* Conversion to strings *)

fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"
  | binOpToString Div   = "/"

(* Some helper functions *)


fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)
fun divi   a b = Op (a, Div, b)

fun assign a b = Assign (a,b)
fun println a = Println (a)

end

(*
1. list of stmts (program)
2. difine stmt datatype
2. list stmt = assign of expr 
            or print of expr

    

*)