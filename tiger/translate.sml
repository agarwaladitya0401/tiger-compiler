structure Translate : sig
val compileExpr: (Temp.temp AtomRedBlackMap.map)* Temp.temp * Ast.Expr -> (Temp.temp AtomRedBlackMap.map) * ((string, Temp.temp) MIPS.stmt list)
val compileStmt: (Temp.temp AtomRedBlackMap.map) * Ast.Stmt -> (Temp.temp AtomRedBlackMap.map) * ((string, Temp.temp) MIPS.stmt list)
val compiled: (Temp.temp AtomRedBlackMap.map) * (Ast.Stmt list) -> ((string, Temp.temp) MIPS.stmt list)
val compile: (Ast.Stmt list) -> ((string, Temp.temp) MIPS.stmt list)
end = struct

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

    | compileStmt (e, Ast.For(x,a,b,(stmts:Ast.Stmt list))) = let
                                                  val new_e = e
                                                  val l = Temp.newlabel()
                                                  val t = Temp.newtemp()
                                                  val new_e = AtomMap.insert(new_e,Atom.atom x,t)
                                                  val c1 = [(IR.li (t, Temp.toTemp(a)))]
                                                  val c2 = [MIPS.Lab (Temp.labelToString(l) ^ ":")]
                                                  val c3 = [IR.bgt(t,Temp.toTemp(b),"exit:")]
                                                  val c4 = [IR.addi(t,t,Temp.toTemp(1))]
                                                  val c5 = compiled(new_e,stmts)
                                                  val c6 = [IR.jump(l)]

                                                  val c7 = [MIPS.Lab "exit:"]
                                                in 
                                                  (e, c1@c2@c3@c4@c5@c6@c7)
                                                 
                                                end



and compiled (e,[])        = []
  | compiled (e, x :: xs) = let 
                              val (en,t) = compileStmt (e,x) 
                              
                            in
                              t @ (compiled (en,xs))
                            end

and compile x = let 
                  val a = Temp.newtemp()
                  val b = Temp.newtemp()
                  val en = AtomMap.insert(e,Atom.atom "a0",a)
                  val env = AtomMap.insert(en,Atom.atom "v0",b)
                in 
                  [MIPS.Dir (MIPS.DATA), MIPS.Dir (MIPS.TEXT), MIPS.Dir (MIPS.GLOBL ("main")), MIPS.Lab "main:"] @ compiled(env,x) @ [(IR.li (b, Temp.toTemp(10))), (MIPS.Inst (MIPS.SYSCALL))]
                
                end


end



