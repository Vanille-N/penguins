type pos = int * int
type 'a grid = 'a array array

type dir = N | NE | SE | S | SW | NW
let all_directions = N :: NE :: SE :: S :: SW :: NW :: []

(* convert a direction into the array index shift it corresponds to *)
let tup_of_dir = function
    | N -> (-1, 0)
    | NE -> (0, 1)
    | SE -> (1, 0)
    | S -> (1, -1)
    | SW -> (0, -1)
    | NW -> (-1, -1)


type move = dir * int

let move (i, j) d =
    let (di, dj) = tup_of_dir d in
    (i + di, j + dj)

let move_n (i, j) (d, n) =
    let (di, dj) = tup_of_dir d in
    (i + di * n, j + dj * n)

(* inclut la position de départ *)
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

let read_grid chan interprete =
    let lines = ref [] in
    (try
        (* seek start *)
        let line = ref "" in
        while !line <> "<problem>" do line := input_line chan done;
        (* read body *)
        while !line <> "</problem>" do
            line := input_line chan;
            lines := !line :: !lines
        done;
        close_in chan
    with End_of_file -> failwith "End of file before termination marker"
    );
    (* determine size of problem *)
    let rec size (i, j) = function
        | [] -> (i, j)
        | line :: rest -> size (i + 1, max (String.length line / 2) j) rest
    in
    let lines = List.rev (List.tl !lines) in (* remove ending + reorder *)
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
    (start, gr)

let from_channel chan = read_grid chan (fun c -> c <> ' ')
let show_problem chan = read_grid chan (fun c -> c)