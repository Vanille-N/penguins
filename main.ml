let () =
    let grid = Hex.show_problem stdin in
    Hex.pp_grid Format.std_formatter (snd grid)

