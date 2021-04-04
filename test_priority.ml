module Ints : (Priority.ORDERED with type t = int) = struct
    type t = int
    let compare a b =
        if a > b then 1
        else if a < b then -1
        else 0
end

module PQueue = Priority.Make(Ints)

let nb_passed = ref 0
let nb_failed = ref 0
let failures = ref []

let test name (tt:unit->unit) =
    Format.printf "  * %s   " name;
    let res =
        try (tt (); true)
        with f -> (
            failures := (name, f) :: !failures;
            false
        )
    in
    if res then (
        incr nb_passed;
        Format.printf
            "%s\x1b[32mOK\x1b[0m\n"
            (String.make (60 - (String.length name)) '.')
    ) else (
        incr nb_failed;
        Format.printf
            "%s\x1b[31mFAILURE\x1b[0m\n"
            (String.make (60 - (String.length name)) '.')
    )

let () = PQueue.(
    Format.printf "\ntest::Priority\n";
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
        let pq = create n 0 0 in
        Array.iter (fun x -> 
            let _ = insert pq x x in ()
        ) arr;
        Array.sort Ints.compare arr;
        let extracted = Array.init n (fun _ -> value (extract_min pq)) in
        test (Format.sprintf "heapsort size %d" n) (fun () ->
            assert (arr = extracted)
        );
    ) [0; 1; 2; 3; 4; 5; 10; 50; 100; 500; 1000];
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
        let xa = insert pq 10 'a' in
        let xb = insert pq 15 'b' in
        let xc = insert pq 20 'c' in
        let xd = insert pq 25 'd' in
        let xe = insert pq 30 'e' in
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
        decrease_key pq xa 3;
        assert (value (extract_min pq) = 'a')
    )
)
       

let () =
    Printf.printf "\n\t\x1b[%dm*-- Summary: %d tests, %d successes, %d failures --*\x1b[0m\n"
        (if !nb_failed = 0 then 32 else 31) (!nb_passed + !nb_failed) !nb_passed !nb_failed;
    List.iter (fun (t,e) ->
        Printf.printf "In test <\x1b[31m%s\x1b[0m>\n" t;
        (
            try raise e
            with    
                | Assert_failure (s, i, j) -> Printf.printf "  Assert failed: \x1b[33m%s at (%d,%d)\x1b[0m\n" s i j
                | Failure s -> Printf.printf "  Failure raised: \x1b[33m%s\x1b[0m\n" s
        )
    ) !failures
