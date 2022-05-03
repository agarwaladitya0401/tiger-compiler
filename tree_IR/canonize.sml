structure Canon 
= struct 

    fun compileExpr (e, t, Tree.CONST x) = (e,[(IR.li (t, Temp.toTemp(x)))])
    | compileExpr (e, t, Tree.NAME x) = (e,[MIPS.Lab (Temp.labelToString (x) ^ ":")])
    | compileExpr (e, t, Tree.TEMP(x)) = (e,[IR.mv (t,x)])
    | compileExpr (e, t, (Tree.BINOP (Tree.PLUS,x,y)))= let 
                                                    val a = Temp.newtemp()
                                                    val b = Temp.newtemp()
                                                    val (en1,e1) = compileExpr (e,a,x)
                                                    val (en2,e2) = compileExpr (en1,b,y)
                                                    val com = e1 @ e2 @ [IR.add (t, a, b)]
                                                in 
                                                    (en2,com)
                                                end

    | compileExpr (e, t, Tree.BINOP (Tree.MINUS,x,y))= let 
                                                    val a = Temp.newtemp()
                                                    val b = Temp.newtemp()
                                                    val (en1,e1) = compileExpr (e,a,x)
                                                    val (en2,e2) = compileExpr (en1,b,y)
                                                    val com = e1 @ e2 @ [IR.sub (t, a, b)]
                                                in 
                                                    (en2,com)
                                                end
    

    | compileExpr (e, t, Tree.BINOP (Tree.MUL,x,y))= let 
                                                    val a = Temp.newtemp()
                                                    val b = Temp.newtemp()
                                                    val (en1,e1) = compileExpr (e,a,x)
                                                    val (en2,e2) = compileExpr (en1,b,y)
                                                    val com = e1 @ e2 @ [IR.mul (t, a, b)]
                                                in 
                                                    (en2,com)
                                                end

    | compileExpr (e,t,Tree.CALL (ex,exs)) = let 
                                        val (en1,e1) = compileExpr(e,t,ex)
                                        in 
                                        (en1,e1)
                                        end
                        
    fun compileStmt (e, Tree.MOVE(e1,e2)) = let 
                                        val a = Temp.newtemp()
                                        val b = Temp.newtemp()
                                        (* val en = AtomMap.insert(e,Atom.atom x,a) *)
                                        val (en1,t1) = compileExpr(e,a,e1)
                                        val (en2,t2) = compileExpr(en1,b,e2)
                                        val com = t1 @ t2 @ [IR.mv (a,b)]
                                        val t = print("df")
                                      in
                                        (en2,com)
                                      end

    fun compileStmt (e, Tree.EXP(e1)) = let 
                                    val a = Temp.newtemp()
                                    val (en1,t1) = compileExpr(e,a,e1)
                                    (* val com = t1 @ [IR.mv (a,t1)] *)
                                    val com = t1
                                      in
                                        (en1,com)
                                      end

    | compileStmt (e, Tree.SEQ(s1,s2)) = let 
                                    val (en1,t1) = compileStmt(e,s1)
                                    val (en2,t2) = compileStmt(en1,s2)
                                    in 
                                      (en2,t1 @ t2)
                                    end

    | compileStmt (e, Tree.LABEL (x)) = (e,[MIPS.Lab (Temp.labelToString (x) ^ ":")])

    | compileStmt (e, Tree.MOVE(e1,e2)) = let 
                                        val a = Temp.newtemp()
                                        val b = Temp.newtemp()
                                        (* val en = AtomMap.insert(e,Atom.atom x,a) *)
                                        val (en1,t1) = compileExpr(e,a,e1)
                                        val (en2,t2) = compileExpr(en1,b,e2)
                                        val com = t1 @ t2 @ [IR.mv (a,b)]
                                      in
                                        (en2,com)
                                      end 
    
    | compileStmt (e, Tree.MOVE(e1,e2)) = (e,[MIPS.Lab ("cmd not found:")])
 
  fun prConst (Tree.TEMP (x1)) = "Tree.TEMP(" ^ Int.toString(Temp.tempToInt(x1))^ ")" 
  | prConst (_) = ""
   

  fun ppStmt(Tree.MOVE(e1,e2)) = print("Tree.MOVE (" ^ prConst(e1) ^ "," ^ prConst(e2)^ ")\n")
  | ppStmt ( _  ) = print ("\n")
      

fun compiled (e,[])        = []
  | compiled (e, x :: xs) = let 
                              val (en,t) = compileStmt (e,x) 
                              (* val tt = ppStmt(x) *)
                            in
                              t @ (compiled (en,xs))
                            end

and compile (e,x) = let 
                        val a = AtomMap.lookup(e,Atom.atom("a0"))
                        val b = AtomMap.lookup(e,Atom.atom("v0"))
                in 
                  [MIPS.Dir (MIPS.DATA), MIPS.Dir (MIPS.TEXT), MIPS.Dir (MIPS.GLOBL ("main")), MIPS.Lab "main:"] @ compiled(e,x) @ [(IR.li (b, Temp.toTemp(10))), (MIPS.Inst (MIPS.SYSCALL))]
                
                end

  
   
      fun pp (e,[]) = ()
    | pp (e,x::xs) = (ppStmt(x); pp (e,xs))

end 