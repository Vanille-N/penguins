let input = ref stdin

type kind = Optimize | Display | Help | File
type arg = End | Error of string | Arg of kind * string

let print_help_general () =
    Format.printf
"pingouin [ARGS] [FILE]
    where ARGS is of
        -o[Optimize-flags]
        -d[Display-flags]
        -h[Help-flags]

"

let print_help_extensive = function
    | Optimize -> Format.printf
"
Optimize:
 1  -> explore with restriction of moves to direct neighbors
 2  -> restrict moves to direct neighbors or furthest away
 3  -> all moves
 T  -> trim positions that can be proved useless to reach
 t  -> trim positions that are useless for single moves
    NOTE: if [3] is not specified then the solution may be invalid
          with regards to the rules of the game
    NOTE: [1],[2],[3] are always executed in that order no matter
          which argument is provided first
    NOTE: [t] is useless if [1] is not provided

    DEFAULT: -o123Tt
"
    | Display -> Format.printf
"
Display:
 D  -> display current progress of search
 G  -> print extensive debug information
 A  -> use ANSI escape sequences for better display
 Q  -> quiet, do not print anything
    NOTE: [A] is disabled if [G] is active
    NOTE: all are useless if [Q] is active
    NOTE: [D],[G],[A] all carry a performance penalty

    DEFAULT: -dDA
"
    | Help -> Format.printf
"
Help:
 O  -> print help about Optimizations
 D  -> print help about Display
 F  -> print help about File
    
    DEFAULT: -hODF
"
    | File -> Format.printf
"
File:
    [FILE]
 foo  -> read problem from file [foo]
    
    DEFAULT: read from stdin

"
let print_help_minimal = function
    | Optimize -> Format.printf "Optimize: -o[123Tt] = -o123Tt\n"
    | Display -> Format.printf "Display: -d[DGAQ] = -dDA\n"
    | Help -> Format.printf "Help: -h[ODF] = -hODF\n"
    | File -> Format.printf "File: [foo] = stdin\n"

let arg_interprete kind flag =
    match kind with
        | Optimize -> ()
        | Display -> ()
        | Help -> ()
        | File -> ()

let () =
    (* Parse command line arguments *)
    let next = (
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
                else if n = 1 then failwith "'-' is not a valid argument"
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
                print_help_minimal Help;
                print_help_minimal File;
                print_help_minimal Display;
                print_help_minimal Optimize;
                exit 255
            )
            | Arg (kind, flags) -> (
                arg_interprete kind flags;
                parse ()
            )
    in
    parse ();
    failwith "End"

let (start, grid) = Hex.from_channel !input

module M : (Paths.S) = struct
    let grid = grid
end

module Path = Paths.Make(M)

let () =
    let (len, moves) = Path.maxpath start in
    let path = Hex.path_of_moves start moves in
    Path.pp_path Format.std_formatter path

