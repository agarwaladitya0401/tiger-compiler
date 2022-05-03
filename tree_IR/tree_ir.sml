structure Tree = struct

     datatype binop = PLUS | MINUS | MUL | DIV 
     datatype relop = EQ | NE | LT | GT | LE 

     datatype expr = CONST of int
		     | NAME  of Temp.label
		     | TEMP  of Temp.temp
			 (* tiger level : variables unbounded number *)
			 (* processor level : processor registers    *)
		     | BINOP of binop * expr * expr
		     | MEM   of expr   (* processor level : an address's content *)
		     | CALL  of expr * expr list
				       (* func (arg1, arg2 ....) *)
		     | ESEQ of stmt * expr

     and stmt = MOVE  of expr * expr    (* e₁ := e₂
					     The value e₂ is stored in the l-value
					     associated with e₁
					   *)
		| EXP   of expr           (* A statement that just evaluates an expression *)

		| JUMP  of expr * Temp.label list
		| CJUMP of relop * expr * expr * Temp.label * Temp.label
		(* CJUMP rop e1 e2 l1 l2  = if e1 rop e2 then goto l1 else goto l2 *)
		| SEQ of stmt * stmt
		| LABEL of Temp.label

     

end