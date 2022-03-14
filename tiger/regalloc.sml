
fun tempToReg 0 =  t0
    | tempToReg 1 = t1
    | tempToReg 2 = t2
    | tempToReg 3 = t3
    | tempToReg 4 = t4
    | tempToReg 5 = t5
    | tempToReg 6 = t6
    | tempToReg 7 = t7
    | tempToReg 8 = s0
    | tempToReg 9 = s1
    | tempToReg 10 = s2
    | tempToReg 11 = s3
    | tempToReg 12 = s4
    | tempToReg 13 = s5
    | tempToReg 14 = s6
    | tempToReg 15 = s7

fun compileInst MIPS.ADD (a,b,c) = MIPS.ADD (tempToReg(a), tempToReg(b), tempToReg(c))
    |  compileInst MIPS.SUB (a,b,c) = MIPS.SUB (tempToReg(a), tempToReg(b), tempToReg(c)) 
    |  compileInst MIPS.MUL (a,b,c) = MIPS.MUL (tempToReg(a), tempToReg(b), tempToReg(c)) 
    |  compileInst MIPS.LI (a,b) = MIPS.ADD (tempToReg(a), tempToInt(b)) 
    |  compileInst MIPS.MOVE (a,b) = MIPS.ADD (tempToReg(a), tempToReg(b)) 
    |  compileInst x = x  

fun reg_alloc []        = []
  | reg_alloc (x :: xs) = compileInst x @ reg_alloc xs
