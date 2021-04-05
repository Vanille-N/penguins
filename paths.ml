module type S = sig
    val grid : bool Hex.grid
end

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
    let accessible set elt = failwith "Unimplemented paths::Make::accessible"
    let disconnected set elt = failwith "Unimplemented paths::Make::disconnected"
    let split set pos = failwith "Unimplemented paths::Make::split"
    let maxpath pos = failwith "Unimplemented paths::Make::maxpath"

end
            
