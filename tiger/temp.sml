signature TEMP =
  sig
     type temp
     val newtemp    : unit -> temp
     val tempToString : temp -> string
     val toTemp : int -> temp
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
              (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   fun newtemp _ = let val t = !nextTemp in nextTemp := t+1; t end
   fun tempToString t = "t" ^ Int.toString t
   fun toTemp x = x 

   (* fun newtemp  _     = complete this, return old nextTemp and incr this*)
   (* fun tempToString t = complete this *)
end

val e = AtomMap.empty
val a = Temp.newtemp()
val b = Temp.newtemp()
val e = AtomMap.insert(e,Atom.atom "a0",a)
val e = AtomMap.insert(e,Atom.atom "v0",b)
val c = AtomMap.lookup(e,Atom.atom("a0"))
val d = AtomMap.lookup(e,Atom.atom("v0"))

(* type mpp = real AtomMap.map 

val t = Temp.newtemp() 
val t1 = Temp.newtemp() 
(* t.temp = 5   *)
(* t1 = 5 *)
a -> 0 = 5
AtomMap.insert(mpp, Atom.atom("a"), t)

x = 5
load then move 
val a = Temp.tempToString t1 *)

