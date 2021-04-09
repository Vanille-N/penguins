let () =
    Arg.(parse
        [("--debug", Unit (fun () -> Config.debug := true), "turn on debug printing");
         ("--display", Unit (fun () -> Config.display := true), "show grid at each step");
         ("--ansi", Unit (fun () -> Config.ansi_fmt := true), "use ANSI escape codes to improve formatting");
    ] (fun s -> ()) "Don't touch my Fish!\nPlease provide the input in stdin")


let (start, grid) = Hex.from_channel stdin 

module M : (Paths.S) = struct
    let grid = grid
end

module Path = Paths.Make(M)

let () =
    let (len, moves) = Path.maxpath start in
    let path = Hex.path_of_moves start moves in
    Path.pp_path Format.std_formatter path

