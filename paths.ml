open Config

module type S = sig
    val grid : bool Hex.grid
end

module Make (M:S) = struct
    (* positions represented by the standard (i,j) <-> i*jmax + j bijection *)
    module Pos : (Bitset.FIN with type t = Hex.pos) = struct
        type t = Hex.pos
        let height = Array.length M.grid
        let width = Array.length M.grid.(0)
        
        let max = height * width
        let to_int (i, j) = i * width + j
        let of_int n = (n / width, n mod width)
    end
    module HSet = Bitset.Make(Pos)

    let print_up ?n:(n=Array.length M.grid + 5) () =
        if !ansi_fmt then Format.printf "\x1b[%dA" n

    let print_down ?n:(n=Array.length M.grid + 5) () =
        if !ansi_fmt then Format.printf "\x1b[%dB" n

    let show_path translator fmt ps =
        let gr = Array.mapi (fun i line ->
            Array.mapi (fun j b ->
                translator (i,j) b
            ) line
        ) M.grid
        in
        (* zip successive positions with list of symbols *)
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

    let pp_path = show_path (fun _ b -> if b then '*' else ' ')

    let translator ice_full ice_trim ice_curr p _ =
        if HSet.member ice_curr p then '*'
        else if HSet.member ice_trim p then '_'
        else if HSet.member ice_full p then '.'
        else ' '

    let all_moves set pos =
        let rec max_reach acc dir n =
            (* how far can one reach in this direction *)
            let mv = (dir, n) in
            let p = Hex.(move_n pos mv) in
            if HSet.(member set p)
            then max_reach (mv :: acc) dir (n + 1)
            else acc
        in
        Hex.all_directions
        |> List.fold_left ( (* repeat for all possible directions *)
            fun (acc:Hex.move list) (dir:Hex.dir) ->
                max_reach acc dir 1
        ) []
        |> inspect ~b:!debug (fun (d,n) -> Format.printf "-> %s x %d\n" Hex.(to_string d) n)

    let single_moves set pos =
        (* moves of the form (_,1) *)
        Hex.all_directions
        |> List.filter (fun d -> HSet.(member set Hex.(move pos d)))
        |> List.map (fun d -> (d, 1))

    let extremal_moves set pos =
        (* moves of the form (_,1) or (_,n) if (_,n+1) is not allowed *)
        let rec max_reach (start:Hex.move) (prev:Hex.move) (dir:Hex.dir) (n:int) =
            let mv = (dir, n) in
            let p = Hex.(move_n pos mv) in
            if HSet.(member set p)
            then max_reach start mv dir (n + 1)
            else if start = mv then []
            else if start = prev then [start]
            else [start; prev]
        in
        Hex.all_directions
        |> List.map (fun d -> max_reach (d,1) (d,1) d 1)
        |> List.flatten
        |> inspect ~b:!debug (fun (d,n) -> Format.printf "-> %s x %d\n" Hex.(to_string d) n)

    (* positions directly adjacent *)
    let neighbors set elt =
        Hex.all_directions
        |> List.map Hex.(move elt)
        |> List.filter HSet.(member set)
        |> inspect ~b:!debug (fun (i,j) -> Format.printf "neighbor (%d,%d) of (%d,%d)\n" i j (fst elt) (snd elt))


    let accessible set elt =
        (* a simple DFS on the set *)
        let rec explore seen = function
            | [] -> seen |> passthrough ~b:!debug (fun x -> Format.printf "%d accessible from (%d,%d)\n" (HSet.cardinal x) (fst elt) (snd elt))
            | pos :: rest when not HSet.(member seen pos) ->
                let adj =
                    neighbors set pos
                    |> inspect ~b:!debug (fun (i,j) -> Format.printf ">>> (%d,%d)\n" i j) 
                    |> List.filter (fun pos -> not (HSet.member seen pos)) (* remove those already explored *)
                in
                explore HSet.(add seen pos) (adj @ rest)
            | _ :: rest -> explore seen rest
        in explore HSet.empty [elt]

    let connected_trivial set elt =
        (* If the set is obviously connected, no need to recalculate ccs *)
        HSet.(cardinal set) = 1 || (
            let has_neighbor mvs mv =
                any (fun m -> List.mem m mvs) Hex.(neighbors mv)
            in
            let neigh = Hex.all_directions |> List.filter (fun d -> HSet.(member set Hex.(move elt d))) in
            all (has_neighbor neigh) neigh
        )
                
    let disconnected set elt =
        if connected_trivial set elt then false
        else (
            (* elt has some neighbors in [set] *)
            let adj = neighbors set elt in
            let new_set = HSet.(remove set elt) in
            let cc_fst = accessible new_set (List.hd adj) in
            HSet.(subset cc_fst new_set)
        )

    let split set elt =
        if connected_trivial set elt then [HSet.(remove set elt)]
        else (
            let adj = neighbors set elt in
            let new_set = HSet.(remove set elt) in
            let ccs =
                List.fold_left (
                    fun (ccs:HSet.t list) (adj:Hex.pos) ->
                        if not (any (fun cc -> HSet.member cc adj) ccs)
                        then (accessible new_set adj) :: ccs
                        else ccs
                ) [] adj
            in ccs
            |> inspect ~b:!debug (fun cc -> Format.printf "Cardinal %d\n" (HSet.cardinal cc))
        )

    let unaligned join cc1 cc2 =
        let d1 = Hex.all_directions |> List.filter (fun d -> HSet.member cc1 Hex.(move join d)) in
        let d2 = Hex.all_directions |> List.filter (fun d -> HSet.member cc2 Hex.(move join d)) in
        match (d1, d2) with
            | [x], [y] when y <> Hex.opposite x -> true
            | _ -> false

    let trim_unreachable startpos ice =
        let trim = ref ice in
        (* when a position splits the board into 3ccs, one must be unreachable *)
        HSet.iter ice (fun p -> (
            let ccs = split ice p
                |> List.sort HSet.compare
            in
            if List.length ccs > 2 then (
                List.iter (fun cc ->
                    HSet.iter cc (fun p ->
                        trim := HSet.remove !trim p;
                        if !debug then Format.printf "removed (first instance) (%d,%d)\n" (fst p) (snd p)
                    )
                ) List.(ccs |> tl |> tl);
            )
        ));
        (* no more than one turning point can be explored to the end *)
        let ice = !trim in
        let turns = ref [] in
        HSet.iter ice (fun p ->
            let ccs = split ice p
                |> List.sort HSet.compare
            in
            if List.length ccs = 2 then (
                let (a, b) = List.((hd ccs, ccs |> tl |> hd)) in 
                if HSet.(member a startpos) && unaligned p a b then (
                    turns := b :: !turns;
                )
            )
        );
        let turns = !turns
            |> List.sort HSet.compare
        in
        if List.length turns > 0 then (
            turns
            |> List.filter (fun cc -> not (HSet.subset cc (List.hd turns)))
            |> List.iter (fun set ->
                HSet.iter set (fun p ->
                    trim := HSet.remove !trim p;
                    if !debug then Format.printf "removed (second instance) (%d,%d)\n" (fst p) (snd p)
                )
            )
        );
        !trim

    let trim_unreachable_single startpos ice =
        let trim = ref ice in
        (* if a position splits the board into two (or 3) ccs, only one of them may be explored *) 
        let turns = ref [] in
        HSet.iter ice (fun p ->
            let ccs = split ice p
                |> List.sort HSet.compare
            in
            if List.length ccs > 1 then (
                let (bg, rest) = List.((hd ccs, tl ccs)) in
                if HSet.(member bg startpos) then (
                    turns := rest @ !turns
                )
            )
        );
        let turns = !turns
            |> List.sort HSet.compare
        in
        if List.length turns > 0 then (
            turns
            |> List.filter (fun cc -> not (HSet.subset cc (List.hd turns)))
            |> List.iter (fun set ->
                HSet.iter set (fun p ->
                    trim := HSet.remove !trim p;
                    if !debug then Format.printf "removed (third instance) (%d,%d)\n" (fst p) (snd p)
                )
            )
        );
        !trim

    type key = int * int * int
    (* (reachable, length, dist):
     * - those with the less wasted space,
     * - then those whose computation is most advanced,
     * - then those with fewer travel distance *)
    type value = HSet.t * Hex.pos * Hex.move list (* (not_sunk, current_pos, path) *)

    module Keys : (Priority.ORDERED with type t = key) = struct
        type t = key
        let compare (r1,l1,d1) (r2,l2,d2) = compare (-r1,-l1,d1) (-r2,-l2,d2) 
    end

    module PQ = Priority.Make(Keys)

    module type SEEN = sig
        type t
        val visit : t -> unit
        val check : t -> bool
        val reset : unit -> unit
    end

    module HMap : (SEEN with type t = HSet.t * Hex.pos) = struct
        type t = HSet.t * Hex.pos
        let seen = Hashtbl.create 100
        let visit x = Hashtbl.add seen x ()
        let check x = not (Hashtbl.mem seen x)
        let reset () = Hashtbl.clear seen
    end

    let maxpath start =
        let ice_full = HSet.init (fun (i,j) -> M.grid.(i).(j)) in
        let ice_trim = trim_unreachable start (accessible ice_full start) in
        let ice_single = trim_unreachable_single start ice_trim in
        let nb = HSet.cardinal ice_trim in
        let best_length = ref 0 in
        let best_moves = ref [] in
        let solution len path =
            if len > !best_length then (
                best_length := len;
                best_moves := path
            )
        in
        if not !quiet then (
            Format.printf "Trimmed useless paths\n";
            show_path (translator ice_full ice_trim ice_trim) Format.std_formatter [];
            Format.printf "Single-move optimization\n";
            show_path (translator ice_full ice_trim ice_single) Format.std_formatter [];
        );
        let bestpath ice_init start allowed_moves =
            let pq = PQ.create 1000000 (0,0,0) (HSet.empty, (0,0), []) in
            ignore PQ.(insert pq (nb, 1, 0) (HSet.(remove ice_init start), start, []));
            while PQ.size pq > 0 do
                flush stdout;
                if not !quiet then Format.printf "===============\nPQ size: %d\n" (PQ.size pq);
                let node = PQ.extract_min pq in
                let (nb, len, dist) = PQ.key node in
                let (ice, pos, path) = PQ.value node in
                solution len path;
                if not !quiet then Format.printf "Reach: %d\nBest: %d\n" HSet.(cardinal ice) !best_length;
                if !debug then (
                    HSet.iter ice (fun (i,j) -> Format.printf "{%d|%d}" i j);
                    Format.printf "\n"
                );
                if !display then show_path (translator ice_full ice_trim ice) Format.std_formatter (Hex.path_of_moves start (List.rev path));
                if not !debug && !display then print_up ();
                if HMap.check (ice, pos) then (
                    HMap.visit (ice, pos);
                    allowed_moves ice pos
                    |> List.map (fun mv -> 
                        let newpos = Hex.move_n pos mv in
                        (mv, List.map (fun x -> (
                            (len + 1 + HSet.cardinal x, len + 1, dist + snd mv),
                            (x, newpos, mv::path)
                        )) (split ice newpos))
                    )
                    |> List.map (fun (mv, acc) -> (
                        if List.length acc = 0
                        then solution (len+1) (mv::path);
                        acc
                    ))
                    |> List.flatten
                    |> inspect ~b:!debug (fun x -> (
                        let ((nb, len, dist), (ice, pos, path)) = x in
                        Format.printf "Visiting (%d,%d) [%d]\n" (fst pos) (snd pos) len
                    ))
                    |> List.filter (fun x -> ( (* remove those whose potential is less than a solution already found *)
                        let ((nb,_,_), _) = x in
                        nb > !best_length
                    ))
                    |> inspect ~b:!debug (fun _ -> Format.printf "1 insertion\n")
                    |> List.iter (fun (k, v) -> ignore PQ.(insert pq k v));
                );
            done
        in
        (* Phase 1 *)
        bestpath ice_single start single_moves;
        if not !quiet then (
            Format.printf "Best path with single moves:\n";
            show_path (translator ice_full ice_trim ice_single) Format.std_formatter (Hex.path_of_moves start (List.rev !best_moves));
        );
        HMap.reset ();
        (* Phase 2 *)
        if not !quiet then Format.printf "Switching to extremal moves\n";
        bestpath ice_trim start extremal_moves;
        if not !quiet then (
            Format.printf "Best path with extremal moves:\n";
            show_path (translator ice_full ice_trim ice_trim) Format.std_formatter (Hex.path_of_moves start (List.rev !best_moves));
        );
        (* Phase 3 *)
        HMap.reset ();
        if not !quiet then Format.printf "Switching to arbitrary moves\n";
        bestpath ice_trim start all_moves;
        print_down ();
        (!best_length, List.rev !best_moves)
end
            
