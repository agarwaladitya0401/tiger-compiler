signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function. *)

val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

val addEdge : 'a graph -> (node * node) -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(* you might want functions that go over all the nodes
maps, folds etc*)
end

structure Graph :> GRAPH  = struct
     type node = word
    structure NodeHashKey : HASH_KEY = struct
        type hash_key = node
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
    end

    structure NodeSet = HashSetFn (NodeHashKey)
    type nodeSet = NodeSet.set
        

    type 'a graph = { labels : (node, 'a)  HashTable.hash_table,
        successors   : (node, nodeSet) HashTable.hash_table,
        predecessors : (node, nodeSet) HashTable.hash_table,
        nextNode : node ref
    }

    fun iden w = w;

    fun empty () = { 
        labels = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),             
        successors = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),
        predecessors = HashTable.mkTable(iden, NodeHashKey.sameKey) (50, Fail "not found"),
        nextNode   = ref (Word.fromInt 0)
    }

    fun newNode (g: 'a graph) (a: 'a) = let 
                                    val nxtNode = !(#nextNode g)
                                  in
                                    HashTable.insert (#labels g) (nxtNode,a);
                                    HashTable.insert (#successors g) (nxtNode,NodeSet.mkEmpty(50));
                                    HashTable.insert (#predecessors g) (nxtNode,NodeSet.mkEmpty(50));
                                    (#nextNode g) := (0w1 + nxtNode);
                                    nxtNode
                                  end  
                                    

    fun addEdge (g: 'a graph) (a,b) = let in
                            NodeSet.add (HashTable.lookup (#successors g) a,b);
                            NodeSet.add (HashTable.lookup (#predecessors g) b,a)
                        end

    fun succ (g: 'a graph) a =  NodeSet.listItems (HashTable.lookup (#successors g) a)
    fun pred (g: 'a graph) a =  NodeSet.listItems (HashTable.lookup (#predecessors g) a)
    fun label (g: 'a graph) a = HashTable.lookup (#labels g) a

    fun clear (g: 'a graph) = let in
                                (#nextNode g) = ref (Word.fromInt 0);
                                HashTable.clear (#successors g);
                                HashTable.clear (#predecessors g);
                                HashTable.clear (#labels g)
                            end
    
    fun nodeList((a,b)::xs) = [a] @ nodeList(xs)
        | nodeList([]) = []

    fun all (g: 'a graph) = nodeList(HashTable.listItemsi (#labels g))

end