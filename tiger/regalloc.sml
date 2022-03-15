structure RA : sig

val compileInst: (string, Temp.temp) MIPS.stmt -> (string, MIPS.reg) MIPS.stmt
val tempToReg: int -> MIPS.reg
val reg_alloc:(string, Temp.temp) MIPS.stmt list -> (string, MIPS.reg) MIPS.stmt list
end = struct
fun tempToReg (0) =  MIPS.t0
    | tempToReg (1) = MIPS.t1
    | tempToReg (2) = MIPS.t2
    | tempToReg (3) = MIPS.t3
    | tempToReg (4) = MIPS.t4
    | tempToReg (5) = MIPS.t5
    | tempToReg (6) = MIPS.t6
    | tempToReg (7) = MIPS.t7
    | tempToReg (8) = MIPS.s0
    | tempToReg (9) = MIPS.s1
    | tempToReg (10) = MIPS.s2
    | tempToReg (11) = MIPS.s3
    | tempToReg (12) = MIPS.s4
    | tempToReg (13) = MIPS.s5
    | tempToReg (14) = MIPS.s6
    | tempToReg (15) = MIPS.s7

(* val compileInst: MIPS.inst -> MIPS.Inst *)
(* a:Temp.temp,b:Temp.temp,c:Temp.temp *)
fun compileInst (MIPS.Inst (MIPS.ADD (a,b,c))) = MIPS.Inst (MIPS.ADD (tempToReg(Temp.tempToInt(a)), tempToReg(Temp.tempToInt(b)), tempToReg(Temp.tempToInt(c))))
    |  compileInst (MIPS.Inst (MIPS.SUB (a,b,c))) = MIPS.Inst (MIPS.SUB (tempToReg(Temp.tempToInt(a)), tempToReg(Temp.tempToInt(b)), tempToReg(Temp.tempToInt(c)))) 
    |  compileInst (MIPS.Inst (MIPS.MUL (a,b,c))) = MIPS.Inst (MIPS.MUL (tempToReg(Temp.tempToInt(a)), tempToReg(Temp.tempToInt(b)), tempToReg(Temp.tempToInt(c))))
    |  compileInst (MIPS.Inst (MIPS.LI (a,b) )) = MIPS.Inst (MIPS.LI (tempToReg(Temp.tempToInt(a)), MIPS.imm(Temp.tempToInt(b)))) 
    |  compileInst (MIPS.Inst (MIPS.MOVE (a,b) )) = MIPS.Inst ( MIPS.MOVE (tempToReg(Temp.tempToInt(a)), tempToReg(Temp.tempToInt(b)))) 
    |  compileInst (MIPS.Dir x ) = MIPS.Dir x 
    |  compileInst (MIPS.Lab x ) = MIPS.Lab x 
    (* |  compileInst _ = raise Error  *)
    (* |  compileInst (MIPS.Inst x ) = MIPS.SYSCALL *)
    (* |  compileInst (MIPS.Inst x ) = (string, MIPS.reg) MIPS.Inst x *)
    (* |  compileInst (x:(string,Temp.temp)MIPS.stmt):(string,MIPS.reg)MIPS.stmt = x:(string,MIPS.reg)MIPS.stmt   *)
    
    fun reg_alloc []        = []
        | reg_alloc (x :: xs) = [compileInst (x)] @ reg_alloc xs

end