(** Argument parsing, help messages, grid initialization *)

(**/**)
let input = ref stdin
let read_from_file = ref false
(**/**)

(** Argument category *)
type kind = Optimize | Display | Help | File

(** Actual argument *)
type arg = End | Error of string | Arg of kind * string

(** {2 Help} *)

(** To be printed whenever any kind of help is provided
 * (be it due to the [-h] flag or because arguments are incorrect) *)
let print_help_general () =
    Format.printf "%s\n%s\n%s\n%s\n%s\n\n"
        "pingouin [ARGS] [FILE]"
        "   where ARGS is of"
        "       -o[Optimize-flags]"
        "       -d[Display-flags]"
        "       -h[Help-flags]"

(** Detailed information, only when explicitly asked for *)
let print_help_extensive = function
    | Optimize -> Format.printf "%s\n%s\n%s\n%s\n\n%s\n\n"
        "Optimize:"
        "  1  -> explore with restriction of moves to direct neighbors"
        "  X  -> restrict moves to direct neighbors or furthest away"
        "  T  -> recursively trim positions than can be eliminated"
        "   DEFAULT: -o1XT"
    | Display -> Format.printf "%s\n%s\n%s\n%s\n%s\n\n%s\n%s\n\n%s\n\n"
        "Display:"
        "  M  -> display major steps of the algorithm"
        "  D  -> display steps of search"
        "  G  -> print debug information"
        "  Q  -> quiet, only print final answer"
        "   NOTE: all are useless if [Q] is active"
        "   NOTE: [D],[G],[M] all carry a performance penalty"
        "   DEFAULT: -dDM"
    | Help -> Format.printf "%s\n%s\n%s\n%s\n%s\n\n%s\n\n"
        "Help:"
        "  H  -> print help about Help (this)"
        "  O  -> print help about Optimizations"
        "  D  -> print help about Display"
        "  F  -> print help about File"
        "   DEFAULT: -hHODF"
    | File -> Format.printf "%s\n%s\n%s\n\n%s\n\n"
        "File:"
        "    [FILE]"
        " foo  -> read problem from file [foo]"   
        "   DEFAULT: read from stdin"

(** Quick reminder for invalid arguments *)
let print_help_minimal () =
    Format.printf "%s\n%s\n%s\n%s\n\n"
        "Optimize: -o[1XT] = -o1XT"
        "Display: -d[DGQM] = -dDM"
        "Help: -h[HODF] = -hHODF"
        "File: [foo] = stdin"

(** {2 Parsing} *)

(** Parse each flag and set options in {!Config} *)
let arg_interprete kind flags =
    match kind with
        | Optimize -> Config.(
            first_pass := false;
            extremal_pass := false;
            trim := false;
            String.iter (function
                | '1' -> first_pass := true
                | 'X' -> extremal_pass := true
                | 'T' -> trim := true
                | c -> (
                    Format.printf "'%c' is invalid for kind Optimize\n\n" c;
                    print_help_general ();
                    print_help_minimal ();
                    print_help_extensive Optimize;
                    exit 1
                )
            ) flags;
        )
        | Display -> Config.(
            debug := false;
            display := false;
            quiet := false;
            show_steps := false;
            String.iter (function
                | 'D' -> display := true
                | 'G' -> debug := true
                | 'Q' -> quiet := true
                | 'M' -> show_steps := true
                | c -> (
                    Format.printf "'%c' is invalid for kind Display\n\n" c;
                    print_help_general ();
                    print_help_minimal ();
                    print_help_extensive Display;
                    exit 1
                )
            ) flags;
            if !quiet then (
                debug := false;
                display := false;
                show_steps := false
            );
            if !debug then display := true;
            if !display then show_steps := true
        )
        | Help -> (
            let none = ref true in
            let fo = ref false in
            let fd = ref false in
            let ff = ref false in
            let fh = ref false in
            String.iter (function
                | 'O' -> (fo := true; none := false)
                | 'D' -> (fd := true; none := false)
                | 'F' -> (ff := true; none := false)
                | 'H' -> (fh := true; none := false)
                | c -> (
                    Format.printf "'%c' is invalid for kind Help\n\n" c;
                    print_help_general ();
                    print_help_minimal ();
                    print_help_extensive Help;
                    exit 1
                )
            ) flags;
            print_help_general ();
            if !fo || !none then print_help_extensive Optimize;
            if !fd || !none then print_help_extensive Display;
            if !ff || !none then print_help_extensive File;
            if !fh || !none then print_help_extensive Help;
            exit 10
        )
        | File -> (
            input := open_in flags;
            read_from_file := true
        )

(* Parse command line arguments *)
let () =
    let next = ( (* yields arguments partially parsed one after another *)
        (* "" -> Error
           () -> End
           "-" -> Error
           "-oXYZ" -> Arg(Optimize, "XYZ")
           "-hXYZ" -> Arg(Help, "XYZ")
           "-dXYZ" -> Arg(Display, "XYZ")
           "FOO" -> Arg(File, "FOO")
           *)
        let i = ref 1 in
        let len = Array.length Sys.argv in
        let next () =
            if !i >= len then End
            else (
                let arg = Sys.argv.(!i) in
                incr i;
                let n = String.length arg in
                if n = 0 then Error "Zero-length argument is invalid"
                else if arg.[0] <> '-' then Arg (File, arg)
                else if n = 1 then Error "'-' is not a valid argument"
                else (
                    let rest = String.sub arg 2 (n-2) in
                    match arg.[1] with
                        | 'o' -> Arg (Optimize, rest)
                        | 'd' -> Arg (Display, rest)
                        | 'h' -> Arg (Help, rest)
                        | c -> Error (Format.sprintf "'%c' is not a valid argument type" c)
                )
            )
        in next
    ) in
    let rec parse () =
        match next () with
            | End -> ()
            | Error msg -> (
                Format.printf "%s\n\n" msg;
                print_help_general ();
                print_help_minimal ();
                exit 1
            )
            | Arg (kind, flags) -> (
                arg_interprete kind flags;
                parse ()
            )
    in
    parse ()

(* Initialize grid and searcher *)
let () =
    if not !read_from_file then (
        print_string "Reading problem from stdin\n";
        flush stdout
    )
let (start, grid) =
    Hex.from_channel !input

module M : (Paths.S) = struct
    let grid = grid
end

module Path = Paths.Make(M)

let () =
    (* computation *)
    let (len, moves) = Path.maxpath start in
    let path = Hex.path_of_moves start moves in
    (* display *)
    if not !Config.quiet
    then Path.pp_path Format.std_formatter path;
    Format.printf "%d\n" (List.length path)

