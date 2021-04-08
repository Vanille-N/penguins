module type S = sig
    val grid : bool Hex.grid
end

let rec any fn = function
    | [] -> false
    | hd :: _ when fn hd -> true
    | _ :: tl -> any fn tl

let inspect fn lst =
    List.map (fun x -> fn x; x) lst

let passthrough fn x =
    fn x; x

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

    let pp_path fmt ps =
        let gr = Array.map (
            Array.map (
                function true -> '*' | _ -> ' '
            )
        ) M.grid
        in
        let rec zipsymb k = function
            | [] -> []
            | hd :: tl -> (
                let symb = char_of_int k in
                let next = match symb with
                    | 'z' -> int_of_char 'A'
                    | 'Z' -> int_of_char '0'
                    | '9' -> int_of_char '?'
                    | '?' -> int_of_char '?'
                    | _ -> k + 1
                in (hd, symb) :: (zipsymb next tl)
            )
        in
        let successive = zipsymb (int_of_char 'a') ps in
        List.iter (fun (pos, symb) ->
            gr.(fst pos).(snd pos) <- symb
        ) successive;
        Hex.pp_grid Format.std_formatter gr

    let all_moves set pos =
        let rec max_reach acc dir n =
            let mv = (dir, n) in
            let p = Hex.(move_n pos mv) in
            if HSet.(member set p)
            then max_reach (mv :: acc) dir (n + 1)
            else acc
        in List.fold_left (
            fun (acc:Hex.move list) (dir:Hex.dir) ->
                max_reach acc dir 1
        ) [] Hex.all_directions
        |> inspect (fun (d,n) -> Format.printf "-> %s x %d\n" Hex.(match d with N -> "N" | S -> "S" | NE -> "NE" | NW -> "NW" | SE -> "SE" | SW -> "SW") n)

    let neighbors set elt =
        Hex.all_directions
        |> List.map Hex.(move elt)
        |> List.filter HSet.(member set)
        |> inspect (fun (i,j) -> Format.printf "neighbor (%d,%d) of (%d,%d)\n" i j (fst elt) (snd elt))

    let accessible set elt =
        let rec explore seen = function
            | [] -> seen |> passthrough (fun x -> Format.printf "%d accessible from (%d,%d)\n" (HSet.cardinal x) (fst elt) (snd elt))
            | pos :: rest when not HSet.(member seen pos) ->
                let adj =
                    neighbors set pos
                    |> inspect (fun (i,j) -> Format.printf ">>> (%d,%d)\n" i j) 
                    |> List.filter (fun pos -> not (HSet.member seen pos)) (* remove those already explored *)
                in
                explore HSet.(add seen pos) (adj @ rest)
            | _ :: rest -> explore seen rest
        in explore HSet.(add empty elt) [elt]
            
    let disconnected set elt =
        if HSet.(cardinal set) = 1 then true
        else (
            (* elt has some neighbors in [set] *)
            let adj = neighbors set elt in
            let new_set = HSet.(remove set elt) in
            let cc_fst = accessible new_set (List.hd adj) in
            HSet.(subset cc_fst new_set)
        )

    let split set elt =
        let adj = neighbors set elt in
        let new_set = HSet.(remove set elt) in
        let ccs =
            List.fold_left (
                fun (ccs:HSet.t list) (adj:Hex.pos) ->
                    if any (fun cc -> not (HSet.member cc adj)) ccs
                    then (accessible new_set adj) :: ccs
                    else ccs
            ) [] adj
        in ccs
        |> inspect (fun cc -> Format.printf "Cardinal %d\n" (HSet.cardinal cc))

    module Keys : (Priority.ORDERED with type t = int * int) = struct
        type t = int * int (* (reachable, length): those with potential, then those whose computation is most advanced *)
        let compare a b = compare b a (* greater (reachable, length) is better *)
    end

    module PQ = Priority.Make(Keys)

    module type SEEN = sig
        type t
        val visit : t -> unit
        val check : t -> bool
    end

    module HMap : (SEEN with type t = value) = struct
        type t = value
        let seen = Hashtbl.create 100
        let visit x = Hashtbl.add seen x ()
        let check x = not (Hashtbl.mem seen x)
    end

        let ice = HSet.init (fun (i,j) -> M.grid.(i).(j)) in
        let nb = HSet.cardinal ice in
        let (pq:(HSet.t * (int * int) * Hex.move list) PQ.queue) = PQ.create nb (0,0) (HSet.empty, (0,0), []) in
        ignore PQ.(insert pq (nb, 1) (ice, pos, []));
        let best_length = ref 0 in
        let best_moves = ref [] in
        while PQ.size pq > 0 do
            let node = PQ.extract_min pq in
            let (nb, len) = PQ.key node in
            let (ice, pos, path) = PQ.value node in
            solution len path;
            Format.printf "Reach: %d\n" HSet.(cardinal ice);
            HSet.iter ice (fun (i,j) -> Format.printf "{%d|%d}" i j); Format.printf "\n";
            pp_path Format.std_formatter (Hex.path_of_moves start (List.rev path));
            if HMap.check (ice, pos, path) then (
                HMap.visit (ice, pos, path);
                all_moves ice pos
                |> passthrough (fun _ -> Format.printf "yes\n")
                |> List.map (fun mv -> 
                    let newpos = Hex.move_n pos mv in
                    (mv, List.map (fun x -> (
                        (len + 1 + HSet.cardinal x, len + 1),
                        (x, newpos, mv::path)
                    )) (split ice newpos))
                )
                |> List.map (fun (mv, acc) -> (
                    if List.length acc = 0
                    then solution (len+1) (mv::path);
                    acc
                ))
                |> List.flatten
                |> inspect (fun x -> (
                    let ((nb, len), (ice, pos, path)) = x in
                    Format.printf "Visiting (%d,%d) [%d]\n" (fst pos) (snd pos) len
                ))
                |> List.filter (fun x -> ( (* remove those whose potential is less than a solution already found *)
                    let ((nb,_), _) = x in
                    nb > !best_length
                ))
                |> inspect (fun _ -> Format.printf "1 insertion\n")
                |> List.iter (fun (k, v) -> ignore PQ.(insert pq k v));
            )
        done;
        (!best_length, List.rev !best_moves)
end
            
