structure Translate =
struct

val e = AtomMap.empty

fun compileExpr (t, Ast.Const x)  =  [(IR.li (t, MIPS.imm x))]
                                 
  | compileExpr (t, Ast.Op (x, Plus, y)) = let a = Temp.newtemp()
                                            b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.add (t, a, b)]
                                        in 
                                            com
                                        end
                                 
  | compileExpr (t, Ast.Op (x, Sub, y)) = let a = Temp.newtemp()
                                            b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.sub (t, a, b)]
                                        in 
                                            com
                                        end
                                 
  | compileExpr (t, Ast.Op (x, Mul, y)) = let a = Temp.newtemp()
                                            b = Temp.newtemp()
                                            val e1 = compileExpr (a,x)
                                            val e2 = compileExpr (b,y)

                                            val com = e1 @ e2 @ [IR.mul (t, a, b)]
                                        in 
                                            com
                                        end

  | compileExpr (t, Variable x) = let a = AtomMap.lookup(mp,Atom.atom(x))
                                    val com = [IR.mv (t,a)]
                                  in
                                    com
                                  end

(* fun comipleStmt (inside call compileExpr)*)

fun comipleStmt (Ast.Println x) = let 
                                      a = Temp.newtemp()
                                      val t = compileExpr(a,x)
                                      val com = t @ [IR.print a]
                                      in
                                        com
                                      end
    | comipleStmt (Ast.Assign x y) = let
                                        a = Temp.newtemp()
                                        b = Temp.newtemp()
                                        val t = compileExpr(b,y)
                                        AtomMap.insert(AtomMap.empty,Atom.atom x,a)
                                        val com = t @ [IR.mv (a,b)]
                                      in
                                        com
                                      end



fun compile []        = []
  | compile (x :: xs) = compileStmt x @ compile xs

end






