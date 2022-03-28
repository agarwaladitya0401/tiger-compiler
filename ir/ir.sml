structure IR : sig
  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (string, Temp.temp) MIPS.stmt
  type prog = stmt list
  (* val ppInst : inst -> unit *)
  val ppStmt : stmt -> unit
  val pp     : prog -> unit
  val add : Temp.temp * Temp.temp  * Temp.temp -> stmt
  (* val imm : Temp.temp -> MIPS.reg *)
  val mul : Temp.temp * Temp.temp * Temp.temp -> stmt
  val sub : Temp.temp * Temp.temp * Temp.temp -> stmt
  val li  : Temp.temp * Temp.temp -> stmt
  val Print : Temp.temp * Temp.temp * Temp.temp -> (string, Temp.temp) MIPS.stmt list
  val mv : Temp.temp * Temp.temp -> stmt
  val bgt : Temp.temp * Temp.temp * string -> stmt
  val addi : Temp.temp * Temp.temp * Temp.temp -> stmt
  val jump : Temp.label -> stmt
  (* val print :  *)
end = struct
  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (string, Temp.temp) MIPS.stmt
  type prog = stmt list
  (* fun imm x = (MIPS.reg imm(x))  *)

  fun add (a,b,c) = MIPS.Inst (MIPS.ADD (a,b,c))
  fun sub (a,b,c) = MIPS.Inst (MIPS.SUB (a,b,c))
  fun mul (a,b,c) = MIPS.Inst (MIPS.MUL (a,b,c))
  fun li (a,b)    = MIPS.Inst (MIPS.LI (a,b))
  fun mv (a,b)    = MIPS.Inst (MIPS.MOVE (a,b))
  fun Print (a:Temp.temp,b:Temp.temp,c:Temp.temp)  = [(mv (a, c)), (li (b, Temp.toTemp(1))), (MIPS.Inst (MIPS.SYSCALL))]
  fun addi (a,b,c) = MIPS.Inst (MIPS.ADDI (a,b,c))
  fun bgt (a,b,c) = MIPS.Inst (MIPS.BGT(a,b,c))
  fun jump(l) = MIPS.Inst (MIPS.J(Temp.labelToString(l)))

  fun ppStmt (MIPS.Inst (MIPS.LI (a,b))) 
              = (print("li "); print(Temp.tempToString(a)^ " ");print(Temp.tempToString(b) ^ " \n"))
      |  ppStmt (MIPS.Inst (MIPS.MOVE (a,b))) 
              = (print("mv "); print(Temp.tempToString(a) ^ " ");print(Temp.tempToString(b) ^ " \n"))
      |  ppStmt (MIPS.Inst (MIPS.J (l))) 
              = (print("j "); print(l ^ " "))
      |  ppStmt (MIPS.Inst (MIPS.BGT (a,b,c))) 
              = (print("bgt "); print(Temp.tempToString(a) ^ " ");print(Temp.tempToString(b) ^ " " ^c  ^" \n"))
    
  fun pp [] = ()
    | pp (x::xs) = (ppStmt(x); pp (xs))

end

