# Summary of Compilers Project

## Lab 0: Preparation.org

-   Just added the basic Readme file containing the roll number and name of mine. And some other basic files just to get familiarize with the environment

---

## Lab 1: Updating the reverse polish compiler

-   In this lab we extended the given reverse polish compiler which was supporting only PLUS { + } and MULTIPLY {\*} and we added DIVISION { / } and BRACKET { ( , ) }
-   All the code and files can be found in the **_reverse-polish_** folder.

### Parsing

-   we parsed the `/`, `(`, `)` signs in `expr.lex`.

    ```sml
    "/"           => ( Tokens.DIV (!lineRef,!lineRef) );
    "("           => ( Tokens.LBRAC (!lineRef,!lineRef) );
    ")"           => ( Tokens.RBRAC (!lineRef,!lineRef) );
    ```

-   After lexing we generated the tokens for the same symbols and called the appropriate function from our `Ast.sml` file

    ```sml
    EX: CONST              ( Ast.Const CONST     )
    | EXP DIV   EXP        ( Ast.div   EXP1 EXP2 )
    | LBRAC EXP RBRAC      ( EXP)
    ```

### Code Generation

-   We converted the tokens into actual symbols in the `Ast.sml` file which is what our compiler understands.

    ```sml
    fun div   a b = Op (a, Div, b)

    fun binOpToString Div   = "/"

    fun binOpDenote Div x y = if(y=0)
                                then (print("division with 0 is not defined "); 0)
                              else
                                x div y
    ```

-   In case division by `0` is encountered we print the error and return 0.
-   Note: We only have to handle `/` operator as brackets doesn't affect our `Ast` structure hence they are handled only at the parsing phase

### Testing

-   Makefile was already present in the reverse polish folder and to test any case we write the test cases in following files:
    -   `test.inp` in form of stack input
        ```sml
            40 40 /p
            (*push 40, two times and then divide then pop the result*)
            (*output: 1*)
        ```
    -   `test.expr` in form of stack input
        ```sml
            (3+3)/2
            56+20
            (*separate lines have separate test cases*)
            (*output: 3*)
            (*output: 76*)
        ```
-   To execute the above files to test our program
    we can change our current directory to _`reverse polish`_ folder then can simply tun `make` in terminal to build our program and then run `make test` in terminal.
-   Output will be visible in the terminal itself.

---

## Lab 2: MIPS assembly as an AST

-   In this lab we have AST of MIPS assembly
    language as supported by SPIM.
-   We have added the following datatype in this lab _`target/mips.sml`_

    -   _reg_ : Registers of the MIPS machine
        ```sml
            datatype reg = zero
                | at
                | v0
                | v1
                .
                .
        ```
    -   _inst_ : Instruction of the MIPS machine

        ```sml
        datatype  ('l,'t) inst = ADD of 't * 't * 't (* add r1 r2 r3 : r1 <- r2 + r3  *)
        	  | ADDI of 't * 't * 't
              | ADDU of 't * 't * 't
              .
              .
        ```

    -   _directive_ : Directives of the MIPS machine

        ```sml
        datatype directive =  ALIGN of int
                | ASCII of string
                | ASCIIZ of string
                .
                .
        ```

    -   _stmt_ : Collection of Instructions, Directives, Labels of the MIPS machine

        ```sml
        datatype ('l,'t) stmt = Inst of ('l, 't) inst
                        | Dir of directive
                        | Lab of string
        ```

    ### Printing functions

    The following are the functions added to print the MIPS instruction which is understand able by the SPIM compiler.

    -   Printing registers of MIPS
        ```sml
        fun printreg (zero)  = "$zero"
          | printreg (at)    = "$at"
          | printreg (v0)   = "$v0"
          | printreg (v1)   = "$v1"
          | ...
        ```
    -   Printing label is easy as they are in form of strings only.

    -   Did some custom printing to make work easier.

        ```sml

        fun customPrintr3 (r1, r2, r3) = printreg(r1) ^ " " ^ printreg(r2) ^ " " ^ printreg(r3)

        fun customPrintr2 (r1, r2) = printreg(r1) ^ " " ^ printreg(r2)

        fun customPrintr1 (r1) = printreg(r1)
        ```

    -   Printing Instruction of MIPS

        ```sml
        fun prInst (ADD  (r1, r2, r3)) = "add "  ^ customPrintr3(r1, r2, r3)
          | prInst (ADDI  (r1, r2, r3)) = "addi "  ^ customPrintr3(r1, r2, r3)
          | prInst (ADDU (r1, r2, r3)) = "addu " ^ customPrintr3(r1, r2, r3)
          | prInst (ADDIU (r1, r2, r3)) = "addiu " ^ customPrintr3(r1, r2, r3)
          | ..
        ```

    -   Some utility functions

        ```sml
        (* printing int list as list of elements in string form  *)
        fun intListToString [] = ""
        | intListToString (x::xs) = (Int.toString x) ^ "," ^ (intListToString xs)

        (* printing string list as list of elements  *)
        fun stringListToString [] = ""
        | stringListToString (x:: xs) = x ^ "," ^ (stringListToString xs)
        ```

    -   Printing Directories of MIPS

        ```sml
        fun prDir (ALIGN (n)) =  ".align " ^ (Int.toString n)
        | prDir (ASCII (s)) =  ".ascii " ^ s
        | prDir (ASCIIZ (s)) =  ".asciiz " ^ s
        | ..
        ```

    -   The following functions is written in order to take the list of MIPS Statements and call appropriate functions to print them

        ```sml
        (* finally printing the statement, which is either instruction or directive *)
        fun prStmt (Inst i) = prInst (i)
            | prStmt (Dir d) = (prDir (d))
            | prStmt (Lab l) = (printlabel (l))

        fun prProg [] =  ""
            | prProg (x::xs) = let
                                    val a = prStmt x
                                in
                                    a ^ "\n" ^ prProg(xs)
                                end
        | ..
        ```

        ### Testing

        -   Change our current directory to the _`target`_ folder in terminal
        -   Run `smlnj mips.sml` and hit enter
        -   If the above command doesn't works try `sml mips.sml`
        -   In the `sml` interactive window type
            ```sml
            open MIPS;
            prInst (ADD (a0, a1, a2))
            prDir (ALIGN (1))
            printlabel ("main:")
            ```

---

## Lab 3: Language with variables

-   In this lab we have build a compiler for a subset of the tiger language which is kind of calculator.
-   In this Lab we Parse the source code of our tiger language
-   Convert the above Parsed code into `Ast` (_`tiger/ast.sml`_) form.
-   Then we convert the above `Ast` into the MIPS program with `IR` (_`ir/ir.sml`_) stage in between.

-   All other the relevant files are located in _`tiger/`_ folder

### Parsing

-   As we are handling only the subset of _tiger_ language, therefore we have added the following functionality in this lab:
    -   Addition
    -   Subtraction
    -   Multiplication
    -   Bracket handling
    -   Assignment
    -   Printing
-   We updated the `tiger.lex` file with the following code to convert the symbols into Tokens.

    ```sml

    "+"           => ( Tokens.PLUS  (!lineRef,!lineRef) );
    "-"           => ( Tokens.MINUS  (!lineRef,!lineRef) );
    "*"           => ( Tokens.MUL (!lineRef,!lineRef) );
    "print"       => ( Tokens.PRINTLN (!lineRef,!lineRef) );
    ":="          => ( Tokens.ASSIGN (!lineRef,!lineRef) );
    "("           => ( Tokens.LBRAC (!lineRef,!lineRef) );
    ")"           => ( Tokens.RBRAC (!lineRef,!lineRef) );
    ```

-   We updated the `tiger.grm` file with the following code to convert the Tokens into our `Ast`.

    ```sml
    EXP : VARIABLE             (Ast.Variable VARIABLE)
        |CONST                 ( Ast.Const CONST     )
        | EXP PLUS EXP         ( Ast.plus  EXP1 EXP2 )
        | EXP MINUS EXP        ( Ast.minus EXP1 EXP2 )
        | EXP MUL   EXP        ( Ast.mul   EXP1 EXP2 )
        | LBRAC EXP RBRAC      ( EXP)

    ```

### IR code generation from `Ast` to `IR`

Here we convert the out `Ast` form to `IR` form by iteration over each instruction of program.

-   The code can be found in _`tiger/translate.sml`_.

```sml
fun compileExpr (e,t, Ast.Const x)  =  (e,[(IR.li (t, Temp.toTemp(x)))])
   | compileExpr (e,t, Ast.Variable (x)) =
                              let
                                  val a = AtomMap.lookup(e,Atom.atom(x))
                                  val com = [IR.mv (t,a)]
                                in
                                  (e,com)
                                end
   | ...

fun compileStmt (e, Ast.Println (x)) =
                        let
                            val a = AtomMap.lookup(e,Atom.atom("a0"))
                            val b = AtomMap.lookup(e,Atom.atom("v0"))
                            val c = Temp.newtemp()
                            val (e,t) = compileExpr(e,c,x)
                            val com = t @ IR.Print (a,b,c)
                            (* utility function to print Const values *)
                            (* fun prConst (Ast.Const x1) = print(Int.toString(x1)^ "\n") *)
                            in
                            (e,com)
                        end
    | ... (*further code can be found in above file*)
```

-   During conversion of `Ast` to `IR` we allocate the registers freely and to this part we used the `temp` registers. Coded in _`tiger/temp.sml`_.
    ```sml
    structure Temp :> TEMP = struct
        type temp  = int
        type label = int
        val nextTemp               = ref 0 (* Keep track of how many temps have been allocated *)
        fun newtemp _              = let val t = !nextTemp in nextTemp := t+1; t end
        val nextLabel              = ref 0
        fun tempToString t         = "t" ^ Int.toString t
        fun newlabel _ = let val t = !nextLabel in nextLabel := t+1; t end
        fun labelToString t        = "l" ^ Int.toString t
        fun toTemp x = x
        fun tempToInt x = x
    end
    ```

### MIPS Assembly Program Generation

We convert the `IR` intermediate representation into our `MIPS` program in the file _`ir/ir.sml`_ file.

-   Defining types of our `MIPS` code
    ```sml
    type inst = (string, Temp.temp) MIPS.inst
    type stmt = (string, Temp.temp) MIPS.stmt
    type prog = stmt list
    ```
-   Functions to convert the `IR` into `MIPS` code.

    ```sml
    fun add (a,b,c) = MIPS.Inst (MIPS.ADD (a,b,c))
    fun sub (a,b,c) = MIPS.Inst (MIPS.SUB (a,b,c))
    fun mul (a,b,c) = MIPS.Inst (MIPS.MUL (a,b,c))
    ... (*further code can be found in above file*)
    ```

-   Functions to pretty print our `MIPS` commands.

    ```sml
     (*printing statement*)
    fun ppStmt (MIPS.Inst (MIPS.LI (a,b)))
            = (print("li "); print(Temp.tempToString(a)^ " ");print(Temp.tempToString(b) ^ " \n"))
    |  ppStmt (MIPS.Inst (MIPS.MOVE (a,b)))
            = (print("mv "); print(Temp.tempToString(a) ^ " ");print(Temp.tempToString(b) ^ " \n"))
    | ... (*further code can be found in above file*)

    (*printing program*)
    fun pp [] = ()
    | pp (x::xs) = (ppStmt(x); pp (xs))
    ```

#### Register Allocation

Here we allocate the register `greedily` whenever we need. To perform this we made the following structure and functions:

```sml
structure RA : sig

val compileInst: (string, Temp.temp) MIPS.stmt -> (string, MIPS.reg) MIPS.stmt
val tempToReg: int -> MIPS.reg
val reg_alloc:(string, Temp.temp) MIPS.stmt list -> (string, MIPS.reg) MIPS.stmt list
end
```

-   The following functions converts the temporary registers to our `MIPS` registers
    ```sml
    fun tempToReg (0) =  MIPS.a0
        | tempToReg (1) = MIPS.v0
        | tempToReg (2) = MIPS.t0
        | tempToReg (3) = MIPS.t1
        | tempToReg (4) = MIPS.t2
        | ....
    ```
-   First and second registers is reserved for `a0` and `v0` respectively.
-   If we run out of registers we simply through error.

### Testing

-   Makefile was already present in the _`tiger`_ folder and to test any case we write the test cases in:
    -   `test.expr` in form of tiger language.
        ```sml
        y := 2
        x := 40 + (y + y)
        print x + y
        (*output: 46*)
        ```
-   To execute the above files to test our program
    we can change our current directory to _`tiger`_ folder then can simply tun `make` in terminal to build our program and then run `make test` in terminal.
-   Output MIPS code will be in `output.mips` file and terminal shows the final output `46` on terminal.

---

## Lab 4: For Loops

-   In this lab we continue building the above compiler for a subset of the tiger language and added the functionality of `for loops`.
-   All files locations and there functionality is same as explained in above lab.

### Parsing

-   As added the functionality of `for loops`, we updated the `tiger.lex` file with the following code to convert the symbols into Tokens.

    ```sml

    "for"         => ( Tokens.FOR (!lineRef,!lineRef) );
    "do"          => ( Tokens.DO (!lineRef,!lineRef) );
    "done"        => ( Tokens.DONE (!lineRef,!lineRef) );
    "="           => ( Tokens.EQUAL (!lineRef,!lineRef) );
    "to"          => ( Tokens.TO (!lineRef,!lineRef) );
    ```

-   We updated the `tiger.grm` file with the following code to convert the Tokens of `for loop` into our `Ast`.

    ```sml
    STMT : PRINTLN EXP          (Ast.println EXP)
    |   VARIABLE ASSIGN EXP     (Ast.assign VARIABLE EXP)
    |   FOR VARIABLE EQUAL CONST TO CONST NEWLINE DO NEWLINE STMTS DONE   (Ast.for VARIABLE CONST1 CONST2 STMTS)

    ```

### IR code generation from `Ast` to `IR`

Here we convert the out `Ast` form to `IR` form by iteration over each instruction of program.

-   The code can be found in _`tiger/translate.sml`_.

```sml
compileStmt (e, Ast.For(x,a,b,(stmts:Ast.Stmt list))) = let

                                val new_e = e
                                val l = Temp.newlabel()
                                val t = Temp.newtemp()
                                val new_e = AtomMap.insert(new_e,Atom.atom x,t)
                                val c1 = [(IR.li (t, Temp.toTemp(a)))]
                                val c2 = [MIPS.Lab (Temp.labelToString(l) ^ ":")]
                                val c3 = [IR.bgt(t,Temp.toTemp(b),"exit")]
                                val c4 = [IR.addi(t,t,Temp.toTemp(1))]
                                val c5 = compiled(new_e,stmts)
                                val c6 = [IR.jump(l)]

                                val c7 = [MIPS.Lab "exit:"]
                            in
                                (e, c1@c2@c3@c5@c4@c6@c7)

                            end
```

-   During conversion of `Ast` to `IR` remains same as above. Coded in _`tiger/temp.sml`_.

### MIPS Assembly Program Generation

We convert the `IR` intermediate representation into our `MIPS` program in the file _`ir/ir.sml`_ file.

-   Defining function types of our `MIPS` code relevant regarding `for loop`
    ```sml
    val bgt : Temp.temp * Temp.temp * string -> stmt
    val addi : Temp.temp * Temp.temp * Temp.temp -> stmt
    val jump : Temp.label -> stmt
    ```
-   Functions relevant regarding `for loop` conversion of `IR` into `MIPS` code.

    ```sml
    fun addi (a,b,c) = MIPS.Inst (MIPS.ADDI (a,b,c))
    fun bgt (a,b,c) = MIPS.Inst (MIPS.BGT(a,b,c))
    fun jump(l) = MIPS.Inst (MIPS.J(Temp.labelToString(l)))
    ... (*further code can be found in above file*)
    ```

-   **Register Allocation remains same**

### Testing

-   Makefile was already present in the _`tiger`_ folder and to test any case we write the test cases in:
    -   `test.expr` in form of tiger language.
        ```sml
        for x = 0 to 10
        do
            print x
        done
        (*output: 012345678910*)
        ```
-   To execute the above files to test our program
    we can change our current directory to _`tiger`_ folder then can simply tun `make` in terminal to build our program and then run `make test` in terminal.
-   Output MIPS code will be in `output.mips` file and terminal shows the final output `012345678910` on terminal.

---

## Lab 5: Tree intermediate representation

-   In this lab we have build a tree intermediate representation for a subset of the tiger language.
-   In this Lab we Parse the source code of our tiger language
-   Convert the above Parsed code into `Ast` (_`tree_IR/ast.sml`_) form.
-   Then we convert the above `Ast` into the MIPS program with `tree_ir` (_`tree_IR/tree_ir.sml`_) stage in between.

-   All other the relevant files are located in _`tree_IR/`_ folder
-   Structure of tree ir is in _`tree_IR/tree_ir.sml`_.

### Parsing

-   Parsing is similar to tiger compiler and currently we support assignment, and some basic arithmetic operations only.

### Tree IR code generation from `Ast` to `Tree IR`

Here we convert the out `Ast` form to `tree IR` form by iteration over each instruction of program.

-   The code can be found in _`tree_IR/translate.sml`_.

```sml
fun compileExpr (e,t, Ast.Const x)  =  (e,[Tree.MOVE (Tree.TEMP(t), (Tree.CONST x))])

  | compileExpr (e,t, Ast.Op ( x ,Plus, y)) =
                                let val a = Temp.newtemp()
                                    val b = Temp.newtemp()
                                    val (en1,e1) = compileExpr (e,a,x)
                                    val (en2,e2) = compileExpr (en1,b,y)

                                    val com = e1 @ e2 @ [Tree.EXP (Tree.BINOP(Tree.PLUS ,Tree.TEMP(a),Tree.TEMP(b)))]
                                in
                                    (en2,com)
                                end
   | ...

```

-   Register allocation remains same here.

### MIPS Assembly Program Generation

We convert the `Tree IR` intermediate representation into our `MIPS` program in the file _`tree_IR/canonize.sml`_ file.

    ```sml
    struct

    fun compileExpr (e, t, Tree.CONST x) = (e,[(IR.li (t, Temp.toTemp(x)))])
    | compileExpr (e, t, Tree.NAME x) = (e,[MIPS.Lab (Temp.labelToString (x) ^ ":")])
    | compileExpr (e, t, Tree.TEMP(x)) = (e,[IR.mv (t,x)])
    | compileExpr (e, t, (Tree.BINOP (Tree.PLUS,x,y)))= let
                                                    val a = Temp.newtemp()
                                                    val b = Temp.newtemp()
                                                    val (en1,e1) = compileExpr (e,a,x)
                                                    val (en2,e2) = compileExpr (en1,b,y)
                                                    val com = e1 @ e2 @ [IR.add (t, a, b)]
                                                in
                                                    (en2,com)
                                                end
    | ...
    ```

### Testing

-   Makefile was already present in the _`tree_IR`_ folder and to test any case we write the test cases in:
    -   `test.expr` in form of tiger language.
        ```sml
        y := 5
        x := 2
        ```
-   To execute the above files to test our program
    we can change our current directory to _`tree_IR`_ folder then can simply tun `make` in terminal to build our program and then run `make test` in terminal.
-   Output MIPS code will be in `output.mips` file.

---

## Lab 6: Basic Blocks

-   In this lab we build the blocks is a sequence of instructions I₁,….,Iₙ such for each 1 ≤ k < n.
-   The blocks are also called `basic blocks` during the execution all the instruction present in one blocks completes wholly then only it jumps to next block. Or we can say is `a maximal block in a control flow graph`.
-   All the relevant files are located in _`basicBlock`_ folder

#### Functionality

-   In this lab we need to design the general functor which takes the following general signature and gives out the list of basic blocks.
    ```sml
    signature INST = sig
        type t   (* The type of the instruction *)
        val isJumpLike   : t -> bool
        val isTarget     : t -> bool
    end
    ```
-   The blocks looks like:
    ```sml
    type block = (string, Temp.temp) MIPS.stmt list
    ```
-   I defined the following utility function to help to find the basic blocks:

    ```sml
    fun utilBasicBlokcs (a::b::c,l,bl) =
                                  if isTarget(a)
                                      then (utilBasicBlokcs(c,[b],bl @ [l]) )
                                  else
                                      if isJumpLike(a)
                                      then utilBasicBlokcs(b::c,[],bl @ [l @ [a]])
                                  else
                                      utilBasicBlokcs(b::c,l@[a],bl)

      | ...
    ```

-   Note that the above `isTarget` and `isJumplike` functions are structure/language dependent which makes our `BasicBlocks` functor general as shown below.

```sml
functor BasicBlocks (I : INST) = struct

    structure Inst = I
    type block = I.t list
    val isJumpLike = I.isJumpLike
    val isTarget = I.isTarget
```

### Basic Blocks for MIPS Program

-   We defined `isJumplike` and `isTarget` functions for the `MIPS` in the following structure.

    ```sml
    structure MIPSInst : INST = struct
          type t = (string, Temp.temp) MIPS.stmt
          fun isJumpLike (MIPS.Inst (MIPS.J (x) )) = true
          | isJumpLike (MIPS.Inst (MIPS.BGT (a,b,c))) = true
          | isJumpLike _ = false

          fun isTarget (MIPS.Lab x) = true
          | isTarget _ =  false
      end
    ```

-   For our tiger compiler which we build above only has `JUMP` and `BGT` as jumpable instructions.
-   And every instruction after the `LABEL` in `MIPS` can be targeted instructions.
-   We call the `BasicBlocks` functor with providing the above definitions of functions.
    ```sml
    structure MIPSBasicBlocks = BasicBlocks (MIPSInst)
    ```

## Lab 7: Graphs for compilers

-   In this lab we build standard ml structure/functor to represent a
    graph to represent the control flow graphs of programs
-   As our graph implementation is expected to have side effects. We have used mutable data structure `HashTable` and functor like `HashSetFn`.
-   Usually the algorithms implemented with `mutable` structures are more efficient.
-   All the relevant files are located in _`graphs`_ folder

#### Functionality

-   In this lab we need to design the graph which has following definitions inside signature. For **mutable graph**.

    ```sml
    val empty   : unit -> 'a graph
    val newNode : 'a graph -> 'a  -> node
    val addEdge : (node * node) -> unit
    val succ    : 'a graph -> node -> node list
    val pred    : 'a graph -> node -> node list
    val label   : 'a graph -> node -> 'a

    val clear   : 'a graph -> unit
    val all     : 'a graph -> node list
    ```

-   We need to define the key type and equality function for our hash, as we are using `Hash Table` for implementation of our graph.
    ```sml
    type node = word
    structure NodeHashKey : HASH_KEY = struct
        type hash_key = node
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
    end
    ```
-   We defined the `empty` function which initializes our graph with the following parameters:
    -   **labels** : Every node has a label (identity) associated with it.
    -   **successors** : All the successors of a particular node for the particular key.
    -   **predecessors** : All the predecessors of a particular node for the particular key.
    -   **nextNode** : Reference to next node from the current node.
    ```sml
    fun empty () = {
        labels = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),
        successors = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),
        predecessors = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),
        nextNode   = ref (Word.fromInt 0)
    }
    ```
    -   Here we indicate that our graph is atmost 50 nodes, if node for key is not found then we through error `not found`.
-   For inserting the new node we make empty `Hash Tables` for size 50 for **successors**, **predecessors** and increment our **nextNode** pointer.

    ```sml
    HashTable.insert (#labels g) (nxtNode,a);
    HashTable.insert (#successors g) (nxtNode,NodeSet.mkEmpty(50));
    HashTable.insert (#predecessors g) (nxtNode,NodeSet.mkEmpty(50));
    (#nextNode g) := (0w1 + nxtNode);
    ```

-   We also add edge in particular order only `addEdge(a,b)` denotes `a-->b`.

```sml
fun addEdge (g: 'a graph) (a,b) =
                    let in
                        NodeSet.add (HashTable.lookup (#successors g) a,b);
                        NodeSet.add (HashTable.lookup (#predecessors g) b,a)
                    end
```

-   Function `succ` returns all the successors for a particular node.

```sml
fun succ (g: 'a graph) a =  NodeSet.listItems (HashTable.lookup (#successors g) a)
```

-   Function `pred` returns all the predecessors for a particular node.

```sml
fun pred (g: 'a graph) a =  NodeSet.listItems (HashTable.lookup (#predecessors g) a)
```

-   Function `label` returns all the label for a particular node.

```sml
fun label (g: 'a graph) a = HashTable.lookup (#labels g) a
```

-   Function `clear` deletes the whole graph.

```sml
HashTable.clear (#successors g);
HashTable.clear (#predecessors g);
HashTable.clear (#labels g)
```

-   Function `label` returns all the label for a particular node.

```sml
fun label (g: 'a graph) a = HashTable.lookup (#labels g) a
```

-   Function `all` returns all the nodes for a graph with the help of utility function `nodeList`.

```sml
fun nodeList((a,b)::xs) = [a] @ nodeList(xs)
    | nodeList([]) = []
fun all (g: 'a graph) = nodeList(HashTable.listItemsi (#labels g))
```

-   The file `graphs/graph.sml` compiles successfully for by the `sml`.

_**TODO's:**_

-   After getting good know knowledge of efficiently assigning registers we can test our graph formed above.
-   if we were not bound to hold a particular structure, more re-usable code can formed for different labs in some common folder.
-   More functionalities can be added for Tree IR.
-   We can add more serious/advance functions to make our compiler for a bigger subset of tiger language.

`Thankyou`
