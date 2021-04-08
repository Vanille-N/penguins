let (start, grid) = Hex.from_channel stdin 

module M : (Paths.S) = struct
    let grid = grid
end

module Path = Paths.Make(M)

let () =
    let (len, moves) = Path.maxpath start in
    let path = Hex.path_of_moves start moves in
    Path.pp_path Format.std_formatter path

