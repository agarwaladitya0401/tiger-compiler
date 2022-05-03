signature TEMP =
  sig
     type temp
     type label
     val newtemp    : unit -> temp
     val tempToString : temp -> string
     val toTemp : int -> temp
     val labelToString : label -> string
     val stringTolabel : string -> label
     val tempToInt : temp -> int
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
              (* you can use IntInf.int if you want unbounded *)

   type label = string
   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   fun newtemp _ = let val t = !nextTemp in nextTemp := t+1; t end
   fun labelToString t = t
   fun stringTolabel t = t
   val nextLabel       = ref 0
   fun tempToString t = "t" ^ Int.toString t
   fun toTemp x = x 
   fun tempToInt x = x

end

(* below is the example of Atom map operations on above Temp structure  *)
(* val e = AtomMap.empty
val a = Temp.newtemp()
val b = Temp.newtemp()
val e = AtomMap.insert(e,Atom.atom "a0",a)
val e = AtomMap.insert(e,Atom.atom "v0",b)
val c = AtomMap.lookup(e,Atom.atom("a0"))
val d = AtomMap.lookup(e,Atom.atom("v0"))
val e = Temp.tempToInt(b)
val f = Temp.toTemp(e) *)


