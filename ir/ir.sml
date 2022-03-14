structure IR : sig
  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (string, Temp.temp) MIPS.stmt
  type prog = stmt list
  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
  val add : Temp.temp * Temp.temp * Temp.temp -> inst
  val imm : Temp.temp -> inst
  val mul : Temp.temp * Temp.temp * Temp.temp -> inst
  val sub : Temp.temp * Temp.temp * Temp.temp -> inst
  val li  : Temp.temp * MIPS.reg -> inst
  val print : Temp.temp -> inst list
  val mv : Temp.temp * Temp.temp -> inst
  (* val print :  *)
end = struct
  
  fun imm x = (MIPS.imm x) 
  fun add (a,b,c) = MIPS.ADD (a,b,c)
  fun sub (a,b,c) = MIPS.SUB a b c
  fun mul (a,b,c) = MIPS.MUL a b c
  fun li (a,b)    = MIPS.LI a b
  fun mv (a,b)    = MIPS.MOVE a b
  fun print a = = [(MIPS.MOVE a0 a), (MIPS.LI v0 1), (MIPS.SYSCALL)]

  (* IR.inst -> list  *)

end

(* (string, MIPS.reg) MIPS.inst *)