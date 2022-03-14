signature TEMP =
  sig
     type temp
     val newtemp    : unit -> temp
     val tempToString : temp -> string
     val toTemp : int -> temp
     val tempToInt : temp -> int
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
              (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   fun newtemp _ = let val t = !nextTemp in nextTemp := t+1; t end
   fun tempToString t = "t" ^ Int.toString t
   fun toTemp x = x 
   fun tempToInt x = x

   (* fun newtemp  _     = complete this, return old nextTemp and incr this*)
   (* fun tempToString t = complete this *)
end

(* below is the example of Atom map operations on above Temp structure  *)
(* val e = AtomMap.empty
val a = Temp.newtemp()
val b = Temp.newtemp()
val e = AtomMap.insert(e,Atom.atom "a0",a)
val e = AtomMap.insert(e,Atom.atom "v0",b)
val c = AtomMap.lookup(e,Atom.atom("a0"))
val d = AtomMap.lookup(e,Atom.atom("v0")) *)



