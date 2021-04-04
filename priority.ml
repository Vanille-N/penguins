module type ORDERED = sig
    type t
    val compare : t -> t -> int
end

module Make (M:ORDERED) = struct
    type 'a node = M.t * 'a
    type 'a queue = {
        mutable size: int;
        contents: 'a node array;
    }

    let create max_size init_key init_value = {
        size = 0;
        contents = Array.make max_size (init_key, init_value);
    }
    
    let size q = q.size

    let sift q n =
        let (key, elem) = q.contents.(n) in
        let rec aux n =
            if n > 0 then (
                let father = (n - 1) / 2 in
                if M.compare (fst q.contents.(father)) key > 0 then (
                    q.contents.(n) <- q.contents.(father);
                    aux father
                ) else n
            ) else 0
        in
        let new_pos = aux n in
        q.contents.(new_pos) <- (key, elem)
        
    let insert q key value =
        let node = (key, value) in
        q.contents.(q.size) <- node;
        q.size <- q.size + 1;
        sift q (q.size - 1);
        node

    let extract_min q =
        if q.size = 0 then failwith "Cannot extract from empty queue";
        let node = q.contents.(0) in
        q.size <- q.size - 1;
        if q.size <> 0 then (
            q.contents.(0) <- q.contents.(q.size);
            trinkle q 0;
        );
        node
    
    let key = fst
    let value = snd
