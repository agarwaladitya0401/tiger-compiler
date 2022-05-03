type block = (string, Temp.temp) MIPS.stmt list
(* val basicBlocks : (string, Temp.temp) MIPS.stmt list -> block list *)

signature INST = sig
    type t   (* The type of the instruction *)
    val isJumpLike   : t -> bool
    val isTarget     : t -> bool
end

functor BasicBlocks (I : INST) = struct

    structure Inst = I                   
    type block = I.t list
    val isJumpLike = I.isJumpLike
    val isTarget = I.isTarget

(* utility function to get block list ('bl') *)
    fun utilBasicBlokcs (a::b::c,l,bl) = if isTarget(a) 
                                            then (utilBasicBlokcs(c,[b],bl @ [l]) ) 
                                        else 
                                            if isJumpLike(a) 
                                            then utilBasicBlokcs(b::c,[],bl @ [l @ [a]]) 
                                        else 
                                            utilBasicBlokcs(b::c,l@[a],bl)

    | utilBasicBlokcs (a::b::[],l,bl) = if isTarget(a) 
                                        then (bl @ [l] @ [[b]] ) 
                                        else 
                                            bl @ [l @ [a] @ [b]]
    | utilBasicBlokcs (a::[],l,bl) =  bl @ [l @ [a]] 
    | utilBasicBlokcs ([],l,bl) = bl

    fun basicBlocks (xs) = utilBasicBlokcs(xs,[],[[]])

end

structure MIPSInst : INST = struct
    type t = (string, Temp.temp) MIPS.stmt
    fun isJumpLike (MIPS.Inst (MIPS.J (x) )) = true
    | isJumpLike (MIPS.Inst (MIPS.BGT (a,b,c))) = true
    | isJumpLike _ = false

    fun isTarget (MIPS.Lab x) = true
    | isTarget _ =  false
end

structure MIPSBasicBlocks = BasicBlocks (MIPSInst)

