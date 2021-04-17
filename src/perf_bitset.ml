(** Very rudimentary measures of performance for operations of {!Priority} *)

module Chars : (Bitset.FIN with type t = char) = struct
    type t = char
    let max = 255
    let to_int = int_of_char
    let of_int = char_of_int
end


module Set = Bitset.Make(Chars)

open Unit_perf

let main () = Set.(
    let n = 10000 in
    let alpha = init (function 'a'..'z' | 'A'..'Z' -> true | _ -> false) in
    perf ~repeat:1000 "add new element" (fun () ->
        for i = 0 to n do
            ignore (add empty 'a')
        done
    );
    perf ~repeat:1000 "add redundant" (fun () ->
        for i = 0 to n do
            ignore (add alpha 'a')
        done
    );
    perf ~repeat:1000 "remove element" (fun () ->
        for i = 0 to n do
            ignore (remove alpha 'a')
        done
    );
    perf ~repeat:1000 "remove useless" (fun () ->
        for i = 0 to n do
            ignore (remove empty 'a')
        done
    );
    let alnum = init (function 'a'..'z' | '0'..'9' -> true | _ -> false) in
    perf ~repeat:100 "union" (fun () ->
        for i = 0 to n do
            ignore (union alpha alnum)
        done
    );
    perf ~repeat:100 "setminus" (fun () ->
        for i = 0 to n do
            ignore (setminus alpha alnum)
        done
    );
    perf ~repeat:100 "intersect" (fun () ->
        for i = 0 to n do
            ignore (intersect alpha alnum)
        done
    )
)

let () =
    init "Bitset";
    main ();
    report ()
