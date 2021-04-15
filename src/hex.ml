type pos = int * int
type 'a grid = 'a array array

type dir = E | NE | SE | W | NW | SW
let all_directions = E :: NE :: SE :: W :: NW :: SW :: []

(* convert move (+line) to difference of positions *)
let move_delta i = function
  | E -> (0, 1)
  | W -> (0, -1)
  | SW -> (1, -(i mod 2))
  | NE -> (-1, (1+i) mod 2)
  | NW -> (-1, -(i mod 2))
  | SE -> (1, (1+i) mod 2)

type move = dir * int

(* now we just have to add each component *)
let move (i, j) d =
    let (di, dj) = move_delta i d in
    (i + di, j + dj)

let rec move_n pos = function
    | (_, 0) -> pos
    | (d, n) -> move_n (move pos d) (d, n-1)

(* includes the starting position *)
let path_of_moves initial moves =
    let rec aux (curr:pos) = function
        | [] -> [curr]
        | mv :: rest -> curr :: aux (move_n curr mv) rest
    in aux initial moves

let pp_grid fmt gr =
    Array.iteri (fun i line -> Format.(
        (* offset if even line *)
        if i mod 2 = 0 then fprintf fmt " ";
        Array.iter (fprintf fmt "%c ") line;
        fprintf fmt "\n"
    )) gr

(* adjacent directions *)
let neighbors = function
    | E -> [NE; SE]
    | W -> [NW; SW]
    | NW -> [NE; W]
    | NE -> [NW; E]
    | SW -> [SE; W]
    | SE -> [SW; E]

(* direction directly opposite *)
let opposite = function
    | E -> W
    | NE -> SW
    | NW -> SE
    | W -> E
    | SW -> NE
    | SE -> NW

let to_string = function
    | E -> "E"
    | W -> "W"
    | NE -> "NE"
    | NW -> "NW"
    | SE -> "SE"
    | SW -> "SW"

let read_grid chan interprete =
    let lines = (
        let rec get_lines acc =
            match input_line chan with
                | "</problem>" -> [] 
                | line -> line :: (get_lines ())
                | exception End_of_file -> failwith "End of file before termination marker"
        and skip_lines () =
            match input_line chan with
                | "<problem>" -> get_lines ()
                | exception End_of_file -> failwith "End of file before beginning marker"
                | _ -> skip_lines ()
        in
        skip_lines ()
    ) in
    (* determine size of problem *)
    let rec size (i, j) = function
        | [] -> (i, j)
        | line :: rest -> size (i + 1, max (String.length line / 2) j) rest
    in
    let (imax, jmax) = size (0, 0) lines in
    let (imax, jmax) = (imax + 2, jmax + 3) in (* padding *)
    let gr = Array.init imax (fun _ -> Array.make jmax (interprete ' ')) in
    (* determine starting point and hex offset *)
    let (start, offset) = (
        let find_start line = String.index_from_opt line 0 '#' in
        let rec aux i = function
            | [] -> failwith "This problem has no starting position"
            | line :: rest -> match find_start line with
                | None -> aux (i+1) rest
                | Some j -> ((i, j), (i + j) mod 2)
        in aux 0 lines
    ) in
    List.iteri (fun i line -> (
        String.iteri (fun j c -> (
            if (i + j + offset) mod 2 == 1 then (
                if c <> ' ' then failwith (
                    Format.sprintf "Position (%d,%d) should be empty" i j
                )
            ) else (
                (* possibly put an ice block *)
                    let target_i = i + 1 in
                    let target_j = j / 2 + 1 + if offset = 0 then (
                        0
                    ) else (
                        target_i mod 2
                    ) in
                    gr.(target_i).(target_j) <- interprete c
            )
        )) line
    )) lines;
    let start = (
        let (i, j) = start in
        let actual_i = i + 1 in
        let actual_j = j / 2 + 1 + (if offset = 0 then 0 else (actual_i mod 2)) in
        (actual_i, actual_j)
    ) in
    (start, gr)

let from_channel chan = read_grid chan (fun c -> c <> ' ')
let show_problem chan = read_grid chan (fun c -> c)
