module Ints : (Priority.ORDERED with type t = int) = struct
    type t = int
    let compare a b =
        if a > b then 1
        else if a < b then -1
        else 0
end

module PQueue = Priority.Make(Ints)

open Unit_test

let main () = PQueue.(
    test "create is empty" (fun () ->
        assert (size (create 10 0 ' ') = 0);
    );
    let pq = create 10 0 ' ' in
    let _ = insert pq 5 'a' in
    let _ = insert pq 3 'b' in
    let _ = insert pq 7 'c' in
    test "insert increments size" (fun () ->
        assert (size pq = 3)
    );
    test "extract yields min" (fun () ->
        assert (value (extract_min pq) = 'b')
    );
    test "extract decrements size" (fun () ->
        assert (size pq = 2)
    );
    test "repeat extract" (fun () ->
        assert (value (extract_min pq) = 'a');
        assert (value (extract_min pq) = 'c')
    );
    test "extract decrements size" (fun () ->
        assert (size pq = 0)
    );
    test "extract from empty" (fun () ->
        assert (
            try let _ = extract_min pq in false
            with _ -> true
        )
    );
    test "still empty after failure" (fun () -> 
        assert (size pq = 0)
    );
    let _ = insert pq 15 '4' in
    let _ = insert pq 28 '5' in
    let _ = insert pq 12 '3' in
    let _ = insert pq 5 '1' in
    let _ = insert pq 7 '2' in
    test "extract still works" (fun () ->
        assert (value (extract_min pq) = '1')
    );
    test "size works" (fun () ->
        assert (size pq = 4)
    );
    test "extract again" (fun () ->
        assert (value (extract_min pq) = '2');
        assert (value (extract_min pq) = '3');
        assert (value (extract_min pq) = '4');
        assert (value (extract_min pq) = '5');
        assert (size pq = 0)
    );
    List.iter (fun n ->
        let arr = Array.init n (fun _ -> Random.int 200) in
        let tmp = create n 0 0 in
        Array.iter (fun x -> 
            let _ = insert tmp x x in ()
        ) arr;
        Array.sort Ints.compare arr;
        let extracted = Array.init n (fun _ -> value (extract_min tmp)) in
        test (Format.sprintf "heapsort size %d" n) (fun () ->
            assert (arr = extracted)
        );
    ) [0; 1; 2; 3; 4; 5; 10; 50; 100; 500(*; 1000*)];
    test "simulate a stack" (fun () ->
        let nmax  = 100 in
        let stk = ref [] in
        let sim = create nmax 0 ' ' in
        let count = ref 0 in
        let pop () =
            let stk_x = List.hd !stk in
            stk := List.tl !stk;
            let sim_x = value (extract_min sim) in
            assert (stk_x = sim_x)
        in
        let push () =
            let x = char_of_int (Random.int 256) in
            stk := x :: !stk;
            let _ = insert sim !count x in 
            decr count;
        in
        for i = 0 to 1000 do
            if size sim = 0 then push ()
            else if size sim = nmax then pop ()
            else if Random.bool () then push ()
            else pop ()
        done
    );
    test "simulate a queue" (fun () ->
        let nmax  = 100 in
        let q = Queue.create () in
        let sim = create nmax 0 ' ' in
        let count = ref 0 in
        let pop () =
            let q_x = Queue.take q in
            let sim_x = value (extract_min sim) in
            assert (q_x = sim_x)
        in
        let push () =
            let x = char_of_int (Random.int 256) in
            Queue.push x q;
            let _ = insert sim !count x in 
            incr count;
        in
        for i = 0 to 1000 do
            if size sim = 0 then push ()
            else if size sim = nmax then pop ()
            else if Random.bool () then push ()
            else pop ()
        done
    );
    test "remove the first" (fun () -> 
        let xc = insert pq 5 'c' in
        let xb = insert pq 3 'b' in
        let xe = insert pq 8 'e' in
        let xa = insert pq 2 'a' in
        let xd = insert pq 6 'd' in
        assert (member pq xa);
        assert (remove pq xa; not (member pq xa));
        assert (member pq xb);
        assert (member pq xc);
        assert (member pq xd);
        assert (member pq xe);
        assert (extract_min pq = xb);
        assert (extract_min pq = xc);
        assert (extract_min pq = xd);
        assert (extract_min pq = xe);
        assert (size pq = 0)
    );
    test "remove the last" (fun () ->
        let xc = insert pq 5 'c' in
        let xb = insert pq 3 'b' in
        let xe = insert pq 8 'e' in
        let xa = insert pq 2 'a' in
        let xd = insert pq 6 'd' in
        assert (member pq xe);
        assert (remove pq xe; not (member pq xe));
        assert (member pq xa);
        assert (member pq xb);
        assert (member pq xc);
        assert (member pq xd);
        assert (extract_min pq = xa);
        assert (extract_min pq = xb);
        assert (extract_min pq = xc);
        assert (extract_min pq = xd);
        assert (size pq = 0)
    );
    test "remove the middle" (fun () ->
        let xc = insert pq 5 'c' in
        let xb = insert pq 3 'b' in
        let xe = insert pq 8 'e' in
        let xa = insert pq 2 'a' in
        let xd = insert pq 6 'd' in
        assert (member pq xc);
        assert (remove pq xc; not (member pq xc));
        assert (member pq xa);
        assert (member pq xb);
        assert (member pq xd);
        assert (member pq xe);
        assert (extract_min pq = xa);
        assert (extract_min pq = xb);
        assert (extract_min pq = xd);
        assert (extract_min pq = xe);
        assert (size pq = 0)
    );
    test "key decreases" (fun () ->
        let xe = insert pq 30 'e' in
        let xa = insert pq 10 'a' in
        let xd = insert pq 25 'd' in
        let xc = insert pq 20 'c' in
        let xb = insert pq 15 'b' in
        assert (decrease_key pq xe 11; extract_min pq = xa);
        assert (value (extract_min pq) = 'e');
        assert (decrease_key pq xc 5; value (extract_min pq) = 'c');
        assert (decrease_key pq xb 13; value (extract_min pq) = 'b');
        assert (extract_min pq = xd);
        assert (size pq = 0)
    );
    test "decrease nonexistent" (fun () ->
        let xa = insert pq 10 'a' in
        assert (extract_min pq = xa);
        assert (not (member pq xa));
        assert (key xa = 10);
        decrease_key pq xa 3;
        assert (key xa = 3);
        assert (member pq xa);
        assert (extract_min pq = xa);
        assert (size pq = 0)
    );
    test "decrease does not invalidate node" (fun () ->
        let xa = insert pq 10 'a' in
        assert (member pq xa);
        assert (key xa = 10);
        decrease_key pq xa 2;
        assert (key xa = 2);
        assert (member pq xa);
        assert (extract_min pq = xa);
        assert (size pq = 0)
    );
    test "duplicate key" (fun () ->
        let _ = insert pq 1 'a' in
        let _ = insert pq 1 'b' in
        let _ = insert pq 1 'c' in
        let x = extract_min pq in
        let y = extract_min pq in
        let z = extract_min pq in
        assert (key x = 1);
        assert (key y = 1);
        assert (key z = 1);
        assert (value x = 'a' || value y = 'a' || value z = 'a');
        assert (value x = 'b' || value y = 'b' || value z = 'b');
        assert (value z = 'c' || value y = 'c' || value z = 'c')
    );
    test "non-copy type" (fun () ->
        let pq = create 10 0 [||] in
        let x1 = insert pq 10 [|1;2;3|] in
        let x2 = insert pq 15 [|0|] in
        let x3 = insert pq 20 [||] in
        let x4 = insert pq 50 [|4;5|] in
        let x5 = insert pq 25 [|1|] in
        assert (member pq x2);
        assert (extract_min pq = x1);
        decrease_key pq x4 5;
        assert (member pq x4);
        assert (extract_min pq = x4);
        assert (not (member pq x4));
        assert (extract_min pq = x2);
        assert (extract_min pq = x3);
        assert (extract_min pq = x5);
        assert (size pq = 0);
    )
)
       

let () =
    init "Priority";
    main ();
    report ()
