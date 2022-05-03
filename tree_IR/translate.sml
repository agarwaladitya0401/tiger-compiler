
structure Translate = struct

val e = AtomMap.empty

fun compileExpr (e,t, Ast.Const x)  =  (e,[Tree.MOVE (Tree.TEMP(t), (Tree.CONST x))])
                                 
  | compileExpr (e,t, Ast.Op ( x ,Plus, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (en1,e1) = compileExpr (e,a,x)
                                            val (en2,e2) = compileExpr (en1,b,y)

                                            val com = e1 @ e2 @ [Tree.EXP (Tree.BINOP(Tree.PLUS ,Tree.TEMP(a),Tree.TEMP(b)))]
                                        in 
                                            (en2,com)
                                        end
                                 
  | compileExpr (e, t, Ast.Op (x, Sub, y)) = let
                                            val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (en1,e1) = compileExpr (e,a,x)
                                            val (en2,e2) = compileExpr (en1,b,y)

                                            val com = e1 @ e2 @ [Tree.EXP (Tree.BINOP(Tree.MINUS,Tree.TEMP(a),Tree.TEMP(b)))]
                                        in
                                            (en2,com)
                                        end
                                 
  | compileExpr (e, t, Ast.Op (x, Mul, y)) = let val a = Temp.newtemp()
                                            val b = Temp.newtemp()
                                            val (en1,e1) = compileExpr (e,a,x)
                                            val (en2,e2) = compileExpr (en1,b,y)

                                            val com = e1 @ e2 @ [Tree.EXP (Tree.BINOP(Tree.MUL,Tree.TEMP(a),Tree.TEMP(b)))]
                                        in 
                                            (en2,com)
                                        end

  | compileExpr (e,t, Ast.Variable (x)) = let 
                                    val a = AtomMap.lookup(e,Atom.atom(x))
                                    val com = [Tree.MOVE (Tree.TEMP(t), Tree.TEMP(a))]
                                  in
                                    (e,com)
                                  end


fun compileStmt (e, Ast.Println (x)) = let
                                      val a = AtomMap.lookup(e,Atom.atom("a0"))
                                      val b = AtomMap.lookup(e,Atom.atom("v0"))
                                      val c = Temp.newtemp()
                                      val (en1,t) = compileExpr(e,c,x)
                                      val s1 = Tree.MOVE (Tree.TEMP a,Tree.TEMP c)
                                      val s2 = Tree.MOVE (Tree.TEMP b,Tree.CONST 1)
                                      val s3 = Tree.LABEL (Temp.stringTolabel("syscall"))
                                      val com = t @ [Tree.SEQ(s1,Tree.SEQ(s2,s3))]
                                      (* utility function to print Const values *)
                                      (* fun prConst (Ast.Const x1) = print(Int.toString(x1)^ "\n") *)
                                      in
                                        (en1,com)
                                      end

    | compileStmt (e, Ast.Assign (x,y)) = let
                                        val a = Temp.newtemp()
                                        val b = Temp.newtemp()
                                        val en = AtomMap.insert(e,Atom.atom x,a)
                                        val (en1,t) = compileExpr(en,b,y)
                                        val com = t @ [Tree.MOVE (Tree.TEMP(a),Tree.TEMP(b))]
                                        (* utility function to print assign values *)
                                        (* fun prVar (x) = print(x ^ "\n") 
                                        fun prConst (Tree.TEMP (x1)) = "Tree.TEMP(" ^ Int.toString(Temp.tempToInt(x1))^ ")" 
                                        fun pr (Tree.MOVE(e1,e2)) = print("Tree.MOVE (" ^ prConst(e1) ^ "," ^ prConst(e2)^ ")\n") *)
                                        
                                      in
                                        (en1,com)
                                      end



and compiled (e,[])        = (e,[])
  | compiled (e, x :: xs) = let 
                              val (en,t) = compileStmt (e,x)
                              val (en1,t1) = compiled(en, xs) 
                            in
                              (en1,t @ t1)
                            end

and compile x = let 
                  val a = Temp.newtemp()
                  val b = Temp.newtemp()
                  val en = AtomMap.insert(e,Atom.atom "a0",a)
                  val env = AtomMap.insert(en,Atom.atom "v0",b)
                in 
                  compiled(env,x)
                end


end



