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

    let rec trinkle q n =
        let (key, elem) = q.contents.(n) in
        let rec aux n =
            let base = 2 * n + 1 in
            let son = if base >= q.size then (
                None
            ) else if base + 1 = q.size then (
                Some base
            ) else if M.compare
                (fst q.contents.(base))
                (fst q.contents.(base + 1))
                <= 0 then (
                Some base
            ) else (
                Some (base + 1)
            ) in match son with
                | None -> n
                | Some s -> (
                    if M.compare
                        key
                        (fst q.contents.(s))
                        > 0 then (
                        q.contents.(n) <- q.contents.(s);
                        aux s
                    ) else n
                )
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

    let find q node =
        let (key, value) = node in
        let rec explore n =
            if n >= q.size then None
            else if q.contents.(n) = node then Some n
            else if M.compare (fst q.contents.(n)) key > 0 then None
            else match explore (2 * n + 1) with
                | None -> explore (2 * n + 2)
                | Some n -> Some n
        in explore 0

    let member q node = match find q node with
        | None -> false
        | Some n -> true

    let remove q node = match find q node with
        | None -> failwith "Cannot remove absent key"
        | Some n -> (
            q.size <- q.size - 1;
            if n < q.size then (
                q.contents.(n) <- q.contents.(q.size);
                sift q n;
                trinkle q n
            )
        )

    let decrease_key q node new_key = match find q node with
        | None -> let _ = insert q new_key (value node) in ()
        | Some n -> (
            q.contents.(n) <- (new_key, value node);
            sift q n
        )
end


