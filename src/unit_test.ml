let nb_failed = ref 0
let nb_passed = ref 0
let failures = ref []
    
let init name =
    Format.printf "\n<%s{ START }%s>\n\n" (String.make 29 '=') (String.make 29 '=');
    Format.printf "test::\x1b[33m%s\x1b[0m\n" name;
    nb_failed := 0;
    nb_passed := 0;
    failures := []

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

let report () =
    Printf.printf
        "\n\t\x1b[%dm*-- Summary: %d tests, %d successes, %d failures --*\x1b[0m\n"
        (if !nb_failed = 0 then 32 else 31)
        (!nb_passed + !nb_failed)
        !nb_passed
        !nb_failed;
    List.iter (fun (t,e) ->
        Printf.printf "In test <\x1b[31m%s\x1b[0m>\n" t;
        (
            try raise e
            with
                | Assert_failure (s, i, j) ->
                    Printf.printf
                        "  Assert failed: \x1b[33m%s at (%d,%d)\x1b[0m\n"
                        s i j
                | Failure s ->
                    Printf.printf
                        "  Failure raised: \x1b[33m%s\x1b[0m\n"
                        s
                | exc -> Printf.printf
                        "  Other exception: \x1b[33m%s\x1b[0m\n"
                        (Printexc.to_string exc)
        )
    ) !failures;
    Format.printf "\n<%s{ END }%s>\n\n" (String.make 30 '=') (String.make 30 '=')
 
