let nb_repeat = 10
let nb_perfs = ref 0

let init name =
    Format.printf "\n<%s{ START }%s>\n\n" (String.make 29 '=') (String.make 29 '=');
    Format.printf "perf::\x1b[33m%s\x1b[0m\n" name;
    nb_perfs := 0

let perf ?repeat:(repeat=nb_repeat) name fn =
    incr nb_perfs;
    Format.printf "  * %s   " name;
    let data = (
        let t_before = Sys.time () in
        for i = 1 to repeat do
            fn ();
        done;
        let t_after = Sys.time () in
        t_after -. t_before
    ) in
    Format.printf "%s \x1b[34m%.5f\x1b[0m (%d iter)\n"
        (String.make (30 - (String.length name)) '.')
        data repeat

let report () =
    Format.printf
        "\n\t\x1b[32m*-- Summary: %d performance measurements --*\x1b[0m\n"
        !nb_perfs;
    Format.printf
        "\n<%s{ END }%s>\n\n"
        (String.make 30 '=')
        (String.make 30 '=')
