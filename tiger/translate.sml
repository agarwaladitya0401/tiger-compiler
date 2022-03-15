structure Translate =
struct

val e = AtomMap.empty

fun compileExpr (e,t, Ast.Const x)  =  (e,[(IR.li (t, Temp.toTemp(x)))])
                                 
  | compileExpr (e,t, Ast.Op ( x ,Plus, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (e,e1) = compileExpr (e,a,x)
                                            val (e,e2) = compileExpr (e,b,y)

                                            val com = e1 @ e2 @ [IR.add (t, a, b)]
                                        in 
                                            (e,com)
                                        end
                                 
  | compileExpr (e, t, Ast.Op (x, Sub, y)) = let
                                            val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (e,e1) = compileExpr (e,a,x)
                                            val (e,e2) = compileExpr (e,b,y)

                                            val com = e1 @ e2 @ [IR.sub (t, a, b)]
                                        in
                                            (e,com)
                                        end
                                 
  | compileExpr (e, t, Ast.Op (x, Mul, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (e,e1) = compileExpr (e,a,x)
                                            val (e,e2) = compileExpr (e,b,y)

                                            val com = e1 @ e2 @ [IR.mul (t, a, b)]
                                        in 
                                            (e,com)
                                        end

  | compileExpr (e,t, Ast.Variable (x)) = let 
                                    val a = AtomMap.lookup(e,Atom.atom(x))
                                    val com = [IR.mv (t,a)]
                                  in
                                    (e,com)
                                  end


fun compileStmt (e, Ast.Println (x)) = let
                                      val a = AtomMap.lookup(e,Atom.atom("a0"))
                                      val b = AtomMap.lookup(e,Atom.atom("v0"))
                                      val c = Temp.newtemp()
                                      val (e,t) = compileExpr(e,c,x)
                                      val com = t @ IR.Print (a,b,c)

                                      (* utility function to print Const values *)
                                      (* fun prConst (Ast.Const x1) = print(Int.toString(x1)^ "\n") *)
                                      in
                                        (e,com)
                                      end

    | compileStmt (e, Ast.Assign (x,y)) = let
                                        val a = Temp.newtemp()
                                        val b = Temp.newtemp()
                                        val en = AtomMap.insert(e,Atom.atom x,a)
                                        val (en,t) = compileExpr(en,b,y)
                                        val com = t @ [IR.mv (a,b)]
                                        (* utility function to print assign values
                                        fun prVar (x) = print(x ^ "\n") 
                                        fun prConst (Ast.Const x1)       = print(Int.toString(x1)^ "\n")  *)
                                      in
                                        (en,com)
                                      end



fun compiled (e,[])        = []
  | compiled (e, x :: xs) = let 
                              val (e,t) = compileStmt (e,x) 
                            in
                              t @ compiled (e,xs)
                            end

fun compile x = let 
                  val a = Temp.newtemp()
                  val b = Temp.newtemp()
                  val en = AtomMap.insert(e,Atom.atom "a0",a)
                  val env = AtomMap.insert(en,Atom.atom "v0",b)
                in 
                  [MIPS.Dir (MIPS.DATA), MIPS.Dir (MIPS.TEXT), MIPS.Dir (MIPS.GLOBL ("main")), MIPS.Lab "main:"] @ compiled(env,x) @ [(IR.li (b, Temp.toTemp(10))), (MIPS.Inst (MIPS.SYSCALL))]
                
                end


end



