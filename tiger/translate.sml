structure Translate =
struct

val e = AtomMap.empty

fun compileExpr (t, Ast.Const x)  =  [(IR.li (t, Temp.toTemp(x)))]
                                 
  | compileExpr (t, Ast.Op ( x ,Plus, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.add (t, a, b)]
                                        in 
                                            com
                                        end
                                 
  | compileExpr (t, Ast.Op (x, Sub, y)) = let
                                            val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.sub (t, a, b)]
                                        in
                                            com
                                        end
                                 
  | compileExpr (t, Ast.Op (x, Mul, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.mul (t, a, b)]
                                        in 
                                            com
                                        end

  | compileExpr (t, Ast.Variable (x)) = let 
                                    val a = AtomMap.lookup(e,Atom.atom(x))
                                    val com = [IR.mv (t,a)]
                                  in
                                    com
                                  end

(* fun comipleStmt (inside call compileExpr)*)

fun compileStmt (Ast.Println (x)) = let 
                                      val a = AtomMap.lookup(e,Atom.atom("a0"))
                                      val b = AtomMap.lookup(e,Atom.atom("v0"))
                                      val c = Temp.newtemp()
                                      val t = compileExpr(c,x)
                                      val com = t @ IR.print (a,b,c)
                                      in
                                        com
                                      end

    | compileStmt (Ast.Assign (x,y)) = let
                                        val a = Temp.newtemp()
                                        val b = Temp.newtemp()
                                        val t = compileExpr(b,y)
                                        val e = AtomMap.insert(e,Atom.atom x,a)
                                        val com = t @ [IR.mv (a,b)]
                                      in
                                        com
                                      end



fun compiled []        = []
  | compiled (x :: xs) = compileStmt x @ compiled xs

fun compile x = let 
                  val a = Temp.newtemp()
                  val b = Temp.newtemp()
                  val e = AtomMap.insert(e,Atom.atom "a0",a)
                  val e = AtomMap.insert(e,Atom.atom "v0",b)
                in 
                  compiled(x)
                end

end


(* (compile calls compile stmt) *)

(* x = 5 
store 5 in temp1
y = x + 2
store 7 in temp2

print x 

load a0 map[temp.temp[x]]
load v0 1
syscall *)

(* print of y, *)



