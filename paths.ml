module type S = sig
    val grid : bool Hex.grid
end

let rec any fn = function
    | [] -> false
    | hd :: _ when fn hd -> true
    | _ :: tl -> any fn tl

module Make (M:S) = struct
    module Pos : (Bitset.FIN with type t = Hex.pos) = struct
        type t = Hex.pos
        let height = Array.length M.grid
        let width = Array.length M.grid.(0)
        
        let max = height * width
        let to_int (i, j) = i * width + j
        let of_int n = (n / width, n mod width)
    end
    module HSet = Bitset.Make(Pos)

    let pp_path fmt ps = failwith "Unimplemented paths::Make::pp_path"
    let all_moves set pos = failwith "Unimplemented paths::Make::all_moves"

    let neighbors set elt =
        Hex.all_directions
        |> List.map Hex.(move elt)
        |> List.filter HSet.(member set)

    let accessible set elt =
        let rec explore seen = function
            | [] -> seen
            | pos :: rest when not HSet.(member seen pos) ->
                let adj =
                    neighbors set pos
                    |> List.filter (fun pos -> not (HSet.member seen pos)) (* remove those already explored *)
                in
                explore HSet.(add seen pos) (adj @ rest)
            | _ :: rest -> explore seen rest
        in explore HSet.(add empty elt) [elt]
            
    let maxpath pos = failwith "Unimplemented paths::Make::maxpath"

end
            
