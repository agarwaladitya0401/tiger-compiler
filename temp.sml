type node = word
structure NodeHashKey : HASH_KEY = struct
        type hash_key = node
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
end
