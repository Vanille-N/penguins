let input = ref stdin

type kind = Optimize | Display | Help | File
type arg = End | Error of string | Arg of kind * string

let print_help =
    List.iter (function
        | Optimize -> Format.printf "opt"
        | Display -> Format.printf "disp"
        | Help -> Format.printf "help"
        | File -> Format.printf "file"
    )

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
                Format.printf "%s\n" msg;
                print_help [Optimize; Display; Help; File]
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

