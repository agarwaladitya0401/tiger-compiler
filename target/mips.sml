structure MIPS = struct

 (* The registers of the mips machine *)
 datatype reg = zero
                | at
                | v0
                | v1
                | a0
                | a1
                | a2
                | a3
                | t0
                | t1
                | t2
                | t3
                | t4
                | t5
                | t6
                | t7
                | s0
                | s1
                | s2
                | s3
                | s4
                | s5
                | s6
                | s7
                | t8
                | t9
                | k0
                | k1
                | gp
                | sp
                | fp
                | ra


 (* The instruction *)
 datatype  ('l,'t) inst = ADD of 't * 't * 't (* add r1 r2 r3   r1 <- r2 + r3  *)
			  | ADDI of 't * 't * 't
              | ADDU of 't * 't * 't
              | ADDIU of 't * 't * 't

              | AND of 't * 't * 't

              | DIV of 't * 't 
              | DIVU of 't * 't 

              | DIV of 't * 't * 't
              | DIVU of 't * 't * 't

              | MUL of 't * 't * 't
              | MULO of 't * 't * 't
              | MULAO of 't * 't * 't
              | MULT of 't * 't 
              | MULTU of 't * 't 
              
              | NEG of 't * 't 
              | NEGU of 't * 't 
              | NOR of 't * 't * 't
              | NOT of 't * 't 

              | OR of 't * 't * 't
              | ORI of 't * 't * 't

              | REM of 't * 't * 't
              | REMU of 't * 't * 't

              | ROL of 't * 't * 't
              | ROR of 't * 't * 't

              | SLL of 't * 't * 't
              | SLLV of 't * 't * 't
              | SRA of 't * 't * 't
              | SRAV of 't * 't * 't
              | SRL of 't * 't * 't
              | SRLV of 't * 't * 't

              | SUB of 't * 't * 't
              | SUBU of 't * 't * 't

              | XOR of 't * 't * 't
              | XORI of 't * 't * 't            (*considered imm as register*)

              | LI of 't * 't
              | LUI of 't * 't

              | SEQ of 't * 't * 't
              | SGE of 't * 't * 't
              | SGEU of 't * 't * 't

              | SGT of 't * 't * 't
              | SGTU of 't * 't * 't
              
              | SLE of 't * 't * 't
              | SLEU of 't * 't * 't

              | SLT of 't * 't * 't
              | SLTI of 't * 't * 't
              | SLTU of 't * 't * 't
              | SLTIU of 't * 't * 't
              | SNE of 't * 't * 't

              | B of 'l
              | BCZT of 'l
              | BCZF of 'l

              | BEQ of 't * 't * 'l
              | BEQZ of 't * 'l

              | BGE of 't * 't * 'l
              | BGEU of 't * 't * 'l

              | BGEZ of 't * 'l
              | BGEZAL of 't * 'l

              | BGT of 't * 't * 'l
              | BGTU of 't * 't * 'l

              | BGTZ of 't * 'l

              | BLE of 't * 't * 'l
              | BLEU of 't * 't * 'l

              (*BGEZAL REPEATED*)  
              | BLTZAL of 't * 'l

              | BLT of 't * 't * 'l
              | BLTU of 't * 't * 'l
              
              | BLTZ of 't * 'l

              | BNE of 't * 't * 'l

              | BNEZ of 't * 'l

              | J of 'l
              | JAL of 'l
              | JALR of 't
              | JR of 't

              | LA of 't * 'l
              | LB of 't * 'l
              | LBU of 't * 'l

              | LD of 't * 'l

              | LH of 't * 'l
              | LHU of 't * 'l

              | LW of 't * 'l

              | LWCZ of 't * 'l

              | LWL of 't * 'l
              | LWR of 't * 'l

              | ULH of 't * 'l
              | ULHU of 't * 'l

              | ULW of 't * 'l

              | SB of 't * 'l
              | SD of 't * 'l
              | SH of 't * 'l
              | SW of 't * 'l

              | SWCZ of 't * 'l
              | SWL of 't * 'l
              | SWR of 't * 'l

              | USH of 't * 'l
              | USW of 't * 'l

              | MFHI of 't * 'l
              | MFLO of 't * 'l

              | MTHI of 't * 'l
              | MTLO of 't * 'l

              | RFE
              | SYSCALL
              | BREAK N
              | NOP


datatype directive =  ALIGN of int 
                    | ASCII of string
                    | ASCIIZ of string
                    | BYTE of int list
                    | DATA
                    | EXTERN of int*int
                    | GLOBL of int 
                    | HALF of int list
                    | KDATA
                    | KTEXT
                    | SPACE of int
                    | TEXT
                    | WORD of string list



 (* The instructions and assembler directives *)
 datatype ('l,'t) stmt = Inst of ('l, 't) inst
                        | Dir of directive



 (* printing the registors in machine understandable form *)

 fun printreg zero  = "$zero"
          | printreg at    = "$at"
          | printreg v0   = "$v0"
          | printreg v1   = "$v1"
          | printreg a0   = "$v1"
          | printreg a1   = "$v1"
          | printreg a2   = "$v1"
          | printreg a3   = "$v1"
          | printreg t0   = "$t0"
          | printreg t1   = "$t1"
          | printreg t2   = "$t2"
          | printreg t3   = "$t3"
          | printreg t4   = "$t4"
          | printreg t5   = "$t5"
          | printreg t6   = "$t6"
          | printreg t7   = "$t7"
          | printreg s0   = "$s0"
          | printreg s1   = "$s1"
          | printreg s2   = "$s2"
          | printreg s3   = "$s3"
          | printreg s4   = "$s4"
          | printreg s5   = "$s5"
          | printreg s6   = "$s6"
          | printreg s7   = "$s7"
          | printreg t8   = "$t8"
          | printreg t9   = "$t9"
          | printreg k0   = "$k0"
          | printreg k1   = "$k1"
          | printreg gp   = "$gp"
          | printreg sp   = "$sp"
          | printreg fp   = "$fp"
          | printreg ra   = "$ra"
          

(* printing registers for functions with 3 or 2 or 1 registers *)
 fun customPrint (r1, r2, r3) = printreg(r1) ^ " " ^ printreg(r2) ^ " " ^ printreg(r3)
     | customPrint (r1, r2) = printreg(r1) ^ " " ^ printreg(r2)
     | customPrint (r1) = printreg(r1)


(* printing registers and label for functions with only label *)
fun customPrintl1 l1) =  printlabel(l1)

(* printing registers and label for functions with 1 register and 1 label *)
fun customPrintl2 (r1, l1) = printreg(r1) ^ " " ^ printlabel(l1) 

(* printing registers and label for functions with 2 registers and 1 label *)
fun customPrintl3 (r1, r2, l1) = printreg(r1) ^ " " ^ printreg(r2) ^ " " ^ printlabel(l1) 


(* printing label *)
fun printlabel (s) =  s

 (* Print the instructions when the labels are strings and
    registers are actual MIPS registers
 *)
 fun prInst ADD  (r1, r2, r3) = "add "  ^ customPrint(r1, r2, r3)     
          | prInst ADDI  (r1, r2, r3) = "addi "  ^ customPrint(r1, r2, r3)
          | prInst ADDU (r1, r2, r3) = "addu " ^ customPrint(r1, r2, r3)
          | prInst ADDIU (r1, r2, r3) = "addiu " ^ customPrint(r1, r2, r3)

          | prInst AND (r1, r2, r3) = "and " ^ customPrint(r1, r2, r3)
          | prInst ANDI (r1, r2, r3) = "andi " ^ customPrint(r1, r2, r3)

          | prInst DIV (r1, r2) = "div " ^ customPrint(r1, r2)
          | prInst DIVU (r1, r2) = "divu " ^ customPrint(r1, r2)

          | prInst DIV (r1, r2, r3) = "div " ^ customPrint(r1, r2, r3)
          | prInst DIVU (r1, r2, r3) = "divu " ^ customPrint(r1, r2, r3)

          | prInst MUL (r1, r2, r3) = "mul " ^ customPrint(r1, r2, r3)
          | prInst MULO (r1, r2, r3) = "mulo " ^ customPrint(r1, r2, r3)
          | prInst MULOU (r1, r2, r3) = "mulou " ^ customPrint(r1, r2, r3)

          | prInst MULT (r1, r2) = "mult " ^ customPrint(r1, r2)
          | prInst MULTU (r1, r2) = "multu " ^ customPrint(r1, r2)

          | prInst NEG (r1, r2) = "neg " ^ customPrint(r1, r2)
          | prInst NEGU (r1, r2) = "negu " ^ customPrint(r1, r2)

          | prInst NOR (r1, r2, r3) = "nor " ^ customPrint(r1, r2, r3)

          | prInst NOT (r1, r2) = "not " ^ customPrint(r1, r2)

          | prInst OR (r1, r2, r3) = "or " ^ customPrint(r1, r2, r3)
          | prInst ORI (r1, r2, r3) = "ori " ^ customPrint(r1, r2, r3)

          | prInst REM (r1, r2, r3) = "rem " ^ customPrint(r1, r2, r3)
          | prInst REMU (r1, r2, r3) = "remu " ^ customPrint(r1, r2, r3)

          | prInst ROL (r1, r2, r3) = "rol " ^ customPrint(r1, r2, r3)
          | prInst ROR (r1, r2, r3) = "ror " ^ customPrint(r1, r2, r3)

          | prInst SLL (r1, r2, r3) = "sll " ^ customPrint(r1, r2, r3)
          | prInst SLLV (r1, r2, r3) = "sllv " ^ customPrint(r1, r2, r3)
          | prInst SRA (r1, r2, r3) = "sra " ^ customPrint(r1, r2, r3)
          | prInst SRAV (r1, r2, r3) = "srav " ^ customPrint(r1, r2, r3)
          | prInst SRL (r1, r2, r3) = "srl " ^ customPrint(r1, r2, r3)
          | prInst SRLV (r1, r2, r3) = "srlv " ^ customPrint(r1, r2, r3)

          | prInst SUB (r1, r2, r3) = "sub " ^ customPrint(r1, r2, r3)
          | prInst SUBU (r1, r2, r3) = "subu " ^ customPrint(r1, r2, r3)

          | prInst XOR (r1, r2, r3) = "xor " ^ customPrint(r1, r2, r3)
          | prInst XORI (r1, r2, r3) = "xori " ^ customPrint(r1, r2, r3)

          | prInst LI (r1, r2) = "li " ^ customPrint(r1, r2)
          | prInst LUI (r1, r2) = "lui " ^ customPrint(r1, r2)

          | prInst SEQ (r1, r2, r3) = "seq " ^ customPrint(r1, r2, r3)

          | prInst SGE (r1, r2, r3) = "sge " ^ customPrint(r1, r2, r3)
          | prInst SGEU (r1, r2, r3) = "sgeu " ^ customPrint(r1, r2, r3)

          | prInst SGT (r1, r2, r3) = "sgt " ^ customPrint(r1, r2, r3)
          | prInst SGTU (r1, r2, r3) = "sgtu " ^ customPrint(r1, r2, r3)

          | prInst SLE (r1, r2, r3) = "sle " ^ customPrint(r1, r2, r3)
          | prInst SLEU (r1, r2, r3) = "sleu " ^ customPrint(r1, r2, r3)

          | prInst SLT (r1, r2, r3) = "slt " ^ customPrint(r1, r2, r3)
          | prInst SLTI (r1, r2, r3) = "slti " ^ customPrint(r1, r2, r3)
          | prInst SLTU (r1, r2, r3) = "sltu " ^ customPrint(r1, r2, r3)
          | prInst SLTIU (r1, r2, r3) = "sltiu " ^ customPrint(r1, r2, r3)

          | prInst SNE (r1, r2, r3) = "sne " ^ customPrint(r1, r2, r3)

          | prInst B (l1) = "b " ^ customPrintl1(l1)

          | prInst BCZT (l1) = "bczt " ^ customPrintl1(l1)
          | prInst BCZF (l1) = "bczf " ^ customPrintl1(l1)

          | prInst BEQ (r1, r2, l1) = "beq " ^ customPrintl3(r1, r2, l1)

          | prInst BEQZ (r1, l1) = "beqz " ^ customPrintl2(r1, l1)

          | prInst BGE (r1, r2, l1) = "bge " ^ customPrintl3(r1, r2, l1)
          | prInst BGEU (r1, r2, l1) = "bgeu " ^ customPrintl3(r1, r2, l1)

          | prInst BGEZ (r1, l1) = "bgez " ^ customPrintl2(r1, l1)
          | prInst BGEZAL (r1, l1) = "bgezal " ^ customPrintl2(r1, l1)

          | prInst BGT (r1, r2, l1) = "bgT " ^ customPrintl3(r1, r2, l1)
          | prInst BGTU (r1, r2, l1) = "bgtu " ^ customPrintl3(r1, r2, l1)

          | prInst BGTZ (r1, l1) = "bgtz " ^ customPrintl2(r1, l1)

          | prInst BLE (r1, r2, l1) = "ble " ^ customPrintl3(r1, r2, l1)
          | prInst BLEU (r1, r2, l1) = "bleu " ^ customPrintl3(r1, r2, l1)

          | prInst BLEZ (r1, l1) = "blez " ^ customPrintl2(r1, l1)

          | prInst BLTZAL (r1, l1) = "bltzal " ^ customPrintl2(r1, l1)

          | prInst BLT (r1, r2, l1) = "blt " ^ customPrintl3(r1, r2, l1)
          | prInst BLTU (r1, r2, l1) = "bltu " ^ customPrintl3(r1, r2, l1)

          | prInst BLTZ (r1, l1) = "bltz " ^ customPrintl2(r1, l1)

          | prInst BNE (r1, r2, l1) = "bne " ^ customPrintl3(r1, r2, l1)

          | prInst BNEZ (r1, l1) = "bnez " ^ customPrintl2(r1, l1)

          | prInst J (l1) = "j " ^ customPrintl1(l1)

          | prInst JAL (l1) = "jal " ^ customPrintl1(l1)

          | prInst JALR (r1) = "jalr " ^ printreg(r1)

          | prInst JR (r1) = "jr " ^ printreg(r1)


          | prInst LA (r1, l1) = "la " ^ customPrintl2(r1, l1)

          | prInst LB (r1, l1) = "lb " ^ customPrintl2(r1, l1)
          | prInst LBU (r1, l1) = "lbu " ^ customPrintl2(r1, l1)

          | prInst LD (r1, l1) = "ld " ^ customPrintl2(r1, l1)


          | prInst LH (r1, l1) = "lh " ^ customPrintl2(r1, l1)
          | prInst LHU (r1, l1) = "lhu " ^ customPrintl2(r1, l1)

          | prInst LW (r1, l1) = "lw " ^ customPrintl2(r1, l1)

          | prInst LWCZ (r1, l1) = "lwcz " ^ customPrintl2(r1, l1)

          | prInst LWL (r1, l1) = "lwl " ^ customPrintl2(r1, l1)
          | prInst LWR (r1, l1) = "lwr " ^ customPrintl2(r1, l1)

          | prInst ULH (r1, l1) = "ulh " ^ customPrintl2(r1, l1)
          | prInst ULHU (r1, l1) = "ulhu " ^ customPrintl2(r1, l1)

          | prInst ULW (r1, l1) = "ulw " ^ customPrintl2(r1, l1)


          | prInst SB (r1, l1) = "sb " ^ customPrintl2(r1, l1)

          | prInst SD (r1, l1) = "sd " ^ customPrintl2(r1, l1)

          | prInst SH (r1, l1) = "sh " ^ customPrintl2(r1, l1)

          | prInst SW (r1, l1) = "sw " ^ customPrintl2(r1, l1)

          | prInst SWCZ (r1, l1) = "swcz " ^ customPrintl2(r1, l1)

          | prInst SWL (r1, l1) = "swl " ^ customPrintl2(r1, l1)
          | prInst SWR (r1, l1) = "swr " ^ customPrintl2(r1, l1)

          | prInst USH (r1, l1) = "ush " ^ customPrintl2(r1, l1)

          | prInst USW (r1, l1) = "usw " ^ customPrintl2(r1, l1)


          | prInst MFHI (r1) = "mfhi " ^ customPrint(r1)
          | prInst MFLO (r1) = "mflo " ^ customPrint(r1)


          | prInst MTHI (r1) = "mthi " ^ customPrint(r1)
          | prInst MTLO (r1) = "mtlo " ^ customPrint(r1)
          

(* printing int list as list of elements in string form  *)
fun intListToString [] = ""
   | intListToString (x: xs) = (Int.toString x) ^ "," ^ (intListToString xs)


(* printing string list as list of elements  *)
fun stringListToString [] = ""
   | stringListToString (x: xs) = x ^ "," ^ (stringListToString xs)

 (* Print the directives *)
fun prDir ALIGN (n) =  ".align " ^ (Int.toString n)
    | prDir ASCII (s) =  ".ascii " ^ s
    | prDir ASCIIZ (s) =  ".asciiz " ^ s
    | prDir BYTE (xs) =  ".byte " ^ intListToString (xs)
    | prDir DATA     =  ".data "
    | prDir EXTERN (a,b) =  ".extern " ^ (Int.toString a) ^ " " ^ (Int.toString b)
    | prDir GLOBL (n) =  ".globl " ^ (Int.toString n)
    | prDir HALF (xs) =  ".half " ^ intListToString (xs)
    | prDir KDATA     =  ".kdata "
    | prDir KTEXT     =  ".ktext "
    | prDir SPACE (n) =  ".space " ^ (Int.toString n)
    | prDir TEXT     =  ".text "
    | prDir WORD (xs) =  ".word " ^ stringListToString (xs)

(* finally printing the statement, which is either instruction or directive *)
fun prStmt (Inst i) = (prInst i)
    | prStmt (Dir d) = (prDir d)

end