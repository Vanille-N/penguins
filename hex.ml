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

