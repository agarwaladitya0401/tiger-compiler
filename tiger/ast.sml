(* The abstract syntax tree for expression *)
structure Ast = struct


datatype Expr  = Const of int
               | Op    of Expr * BinOp * Expr
               | Variable of string
and BinOp = Plus
               | Sub
               | Mul
               (* | Div *)

datatype Stmt = Println of Expr
              | Assign of string*Expr

               

(* type mpp = real AtomMap.map 
     


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
  | binOpToString Div   = "/" *)

(* Some helper functions *)

(* utility functions to print expressions *)
fun prConst (Const x)       = print(Int.toString(x))

fun printProg (Assign (a,b)) = (print("assign");print(a);prConst(b))
    | printProg _ = print("0\n")

fun progList [] = ()
    | progList (x::xs) = (printProg (x); progList(xs))

fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Sub, b)
fun mul   a b = Op (a, Mul, b)
(* fun divi   a b = Op (a, Div, b) *)
(* fun const a = (print(Int.toString(a)); Const a) *)

fun assign a b = Assign (a,b)
fun println a = Println (a)

end

