module Cfg = Config

module type S = sig
    val grid : bool Hex.grid
end

let rec any fn = function
    | [] -> false
    | hd :: _ when fn hd -> true
    | _ :: tl -> any fn tl

let rec all fn = function
    | [] -> true
    | hd :: _ when not (fn hd) -> false
    | _ :: tl -> all fn tl

module Make (M:S) = struct
    (* positions represented by the canonical (i,j) <-> i*width+j bijection *)
    module Pos : (Bitset.FIN with type t = Hex.pos) = struct
        type t = Hex.pos
        let height = Array.length M.grid
        let width = Array.length M.grid.(0)
        
        let max = height * width
        let to_int (i, j) = i * width + j
        let of_int n = (n / width, n mod width)
    end
    module HSet = Bitset.Make(Pos)

    let show_path translator fmt ps =
        (* convert to printable characters *)
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
        (* write chars that represent path *)
        let successive = zipsymb (int_of_char 'a') ps in
        List.iter (fun (pos, symb) ->
            gr.(fst pos).(snd pos) <- symb
        ) successive;
        Hex.pp_grid Format.std_formatter gr

    let pp_path = show_path (fun _ b -> if b then '*' else ' ')

    (* pretty-printing: '*' for available, ' ' for empty, '.' for unreachable *)
    let translator ice_full ice_curr p _ =
        if HSet.member ice_curr p then '*'
        else if HSet.member ice_full p then '.'
        else ' '

    let all_moves set pos =
        let rec max_reach pos dir n =
            (* how far can one reach in this direction *)
            let mv = (dir, n) in
            let p = Hex.(move pos dir) in
            if HSet.(member set p)
            then mv :: (max_reach p dir (n + 1))
            else []
        in
        Hex.all_directions
        |> List.map (fun dir -> max_reach pos dir 1)
        |> List.flatten
        |> Cfg.(inspect ~b:!debug (fun (d,n) -> Format.printf "-> %s x %d\n" Hex.(to_string d) n))

    let single_moves set pos =
        (* moves of the form (_,1) *)
        Hex.all_directions
        |> List.filter (fun d -> HSet.(member set Hex.(move pos d)))
        |> List.map (fun d -> (d, 1))

    let extremal_moves set pos =
        (* moves of the form (_,1) or (_,n) when (_,n+1) is not allowed *)
        let rec max_reach pos start prev dir n =
            let mv = (dir, n) in
            let p = Hex.(move pos dir) in
            if HSet.(member set p)
            then max_reach p start mv dir (n + 1)
            else if start = mv then []
            else if start = prev then [start]
            else [start; prev]
        in
        Hex.all_directions
        |> List.map (fun d -> max_reach pos (d,1) (d,1) d 1)
        |> List.flatten
        |> Cfg.(inspect ~b:!debug (fun (d,n) -> Format.printf "-> %s x %d\n" Hex.(to_string d) n))

    (* positions directly adjacent *)
    let neighbors set elt =
        Hex.all_directions
        |> List.map Hex.(move elt)
        |> List.filter HSet.(member set)

    let accessible set elt =
        (* This one is trivial because the algorithm was moved
           to Bitset.ml for performance reasons *)
        HSet.transitive_closure elt (neighbors set)

    let connected_trivial set elt =
        (* Criteria for 'trivial': all neighbors of [elt] have an adjacent direction also a neighbor
           Examples: (elt = '#', neighbor with adjacent = 'A', neighbor without adjacent = 'N')
            
             x                                x x x
              x A A                          x A   x
               A # A                          A # N

             trivial                        nontrivial
           
           In other words, [remove set elt] is trivially connected when [intersect set (neigbors elt)] is connected
           *)
        HSet.(cardinal set) = 1 || (
            let has_neighbor mvs mv =
                any (fun m -> List.mem m mvs) Hex.(neighbors mv)
            in
            let neigh = Hex.all_directions
                |> List.filter (fun d -> HSet.(member set Hex.(move elt d)))
            in
            all (has_neighbor neigh) neigh
        )
                
    let disconnected set elt =
        if connected_trivial set elt then false
        else (
            (* [elt] has some neighbors in [set]
               see if CC of first neighbor is the whole set
               *)
            let adj = neighbors set elt in
            let new_set = HSet.(remove set elt) in
            let cc_fst = accessible new_set (List.hd adj) in
            HSet.(subset cc_fst new_set)
        )

    let split set elt =
        if connected_trivial set elt then [HSet.(remove set elt)] (* don't recalculate everything *)
        else (
            let adj = neighbors set elt in
            let new_set = HSet.(remove set elt) in
            let ccs =
                List.fold_left (
                    fun (ccs:HSet.t list) (adj:Hex.pos) ->
                        (* check that [adj] is not already part of another CC *)
                        if not (any (fun cc -> HSet.member cc adj) ccs)
                        then (accessible new_set adj) :: ccs
                        else ccs
                ) [] adj
            in ccs
            |> Cfg.(inspect ~b:!debug (fun cc -> Format.printf "Cardinal %d\n" (HSet.cardinal cc)))
        )

    type key = int * int 
    (* (reachable, length):
     * - those with the less wasted space,
     * - then those whose computation is most advanced *)
    type value = HSet.t * Hex.pos * Hex.move list (* (not_sunk, current_pos, path) *)

    module Keys : (Priority.ORDERED with type t = key) = struct
        type t = key
        let compare k k' = compare k' k
    end

    module PQ = Priority.Make(Keys)

    module type SEEN = sig
        (* record of already encountered configurations
           NOTE: state-dependant module. Do not use as if it were immutable
           *)
        type t
        val visit : t -> unit
        val check : t -> bool
        val reset : unit -> unit
    end

    module HMap : (SEEN with type t = HSet.t * Hex.pos) = struct
        type t = HSet.t * Hex.pos
        let seen = Hashtbl.create 10000 (* arbitrary *)
        let visit x = Hashtbl.add seen x ()
        let check x = not (Hashtbl.mem seen x)
        let reset () = Hashtbl.clear seen
    end

    type best = {
        mutable len : int;
        mutable path : Hex.move list;
    }

    let maxpath start =
        let ice_full = accessible HSet.(init (fun (i,j) -> M.grid.(i).(j))) start in
        let nb = HSet.cardinal ice_full in
        let positions = HSet.collect ice_full in
        let board_splits = if !Cfg.trim then (
            positions
            |> List.map (fun p ->
                let parts = split ice_full p in
                let cc_start = parts
                    |> List.filter (fun cc -> HSet.(member cc start))
                in
                let cc_rest = parts
                    |> List.filter (fun cc -> not HSet.(member cc start))
                in
                (p, cc_start, cc_rest)
            )
            |> List.filter (function
                | (p, _, _) when p = start -> false (* don't split on initial position *)
                | (_, [_], [_;_]) -> true
                    (* has to be in a configuration like

                                      x               <-.
                                     x                   \
                   start ->     x x #    <- split         |- only one of these two
                                     x                   /   branches can be reached
                                      x               <-'
                     *)
                | (p, [s], [r]) -> not (
                    (* detect if configuration is like

                                      x               <- accessing this branch
                                     x                   prevents going back to
                   start ->     x x #    <- split        the initial zone   
   
                     *)
                    Hex.all_directions
                    |> List.map (fun mv -> (Hex.move p mv, Hex.(move p (opposite mv))))
                    |> any (fun (a,b) -> HSet.(member s a && member r b))
                )
                | _ -> false
            )
            |> List.map (function
                | (p, _, [r]) -> (p, HSet.add r p)
                | (p, _, [r1;r2]) -> (p, HSet.add (HSet.union r1 r2) p)
                | _ -> failwith "Unreachable @ Paths::Make::maxpath"
            )
            |> List.filter (function (_, s) -> not (HSet.cardinal s = 1))
            |> List.sort (fun (_,cc) (_,cc') -> HSet.compare cc' cc) (* smallest first *)
        ) else []
        in
        let (precalculated: (Hex.pos * HSet.t, best) Hashtbl.t) =
            Hashtbl.create 100 (* arbitrary *)
        in
        if !Cfg.show_steps then board_splits
            |> List.iter (fun (p,s) -> show_path (translator ice_full s) Format.std_formatter []);
        (* 1_000_000 is a deliberately fixed size: requiring a bigger priority queue is a sign that
         * some useless configurations are not being properly trimmed *)
        let pq = PQ.create 1000000 (0,0) (HSet.empty, (0,0), []) in
        let bestpath best ice_init start allowed_moves =
            HMap.reset ();
            if PQ.size pq <> 0 then failwith "Queue was not properly emptied"; (* just to make sure *)
            ignore PQ.(insert pq (nb, 1) (HSet.(remove ice_init start), start, []));
            while PQ.size pq > 0 do
                flush stdout;
                if !Cfg.display then Format.printf "===============\nPQ size: %d\n" (PQ.size pq);
                let node = PQ.extract_min pq in
                let (nb, len) = PQ.key node in
                let (ice, pos, path) = PQ.value node in

                (* improvement ? *)
                if len > best.len then (
                    best.len <- len;
                    best.path <- path;
                );
                if !Cfg.display then Format.printf "Reach: %d\nBest: %d\n" HSet.(cardinal ice) best.len;
                if !Cfg.debug then (
                    Format.printf "Reachable: ";
                    HSet.iter ice (fun (i,j) -> Format.printf "{%d|%d}" i j);
                    Format.printf "\n"
                );
                if !Cfg.display then show_path
                    (translator ice_full ice)
                    Format.std_formatter
                    (path |> List.rev |> Hex.(path_of_moves start));
                if !Cfg.trim && Hashtbl.mem precalculated (pos,ice) then (
                    (* this configuration is already precalculated *)
                    let entry = Hashtbl.find precalculated (pos,ice) in
                    if entry.len + len - 1 > best.len then (
                        best.len <- entry.len + len - 1; (* -1 is because the junction must not be counted twice *)
                        best.path <- entry.path @ path;
                    );
                ) else if HMap.check (ice, pos) then (
                    (* this configuration was never encountered *)
                    HMap.visit (ice, pos);
                    allowed_moves ice pos
                    |> List.map (fun mv ->  (* split the set on the position *)
                        let newpos = Hex.move_n pos mv in
                        (
                            mv,
                            split ice newpos
                            |> List.map (fun x -> (
                                (len + 1 + HSet.cardinal x, len + 1),
                                (x, newpos, mv::path)
                             ))
                        )
                    )
                    |> List.map (fun (mv, acc) -> ( (* check for an improvement *)
                        if List.length acc = 0
                        && len >= best.len then (
                            best.len <- len + 1;
                            best.path <- mv :: path;
                        );
                        acc
                    ))
                    |> List.flatten
                    |> Cfg.(inspect ~b:!debug (fun x -> (
                        let ((nb, len), (ice, pos, path)) = x in
                        Format.printf "Visiting (%d,%d) [%d]\n" (fst pos) (snd pos) len
                    )))
                    |> List.filter (fun x -> ( (* remove those whose potential is less than a solution already found *)
                        let ((nb,_), _) = x in
                        nb > best.len
                    ))
                    |> Cfg.(inspect ~b:!debug (fun _ -> Format.printf "1 insertion\n"))
                    |> List.iter (fun (k, v) -> ignore PQ.(insert pq k v))
                )
            done
        in
        let ice_trim = ref ice_full in
        if !Cfg.trim then (
            board_splits
            |> List.iter (fun (p,ice) ->
                let best = { len=0; path=[]; } in
                (* remove useless positions as explained in README (section Optimizations, subsection T) *)
                bestpath best HSet.(intersect ice !ice_trim) p all_moves;
                Hashtbl.add precalculated (p,HSet.remove ice p) best;
                let useful_pos = Hex.path_of_moves p (List.rev best.path) in
                let useful_set = HSet.of_list useful_pos in
                let useless_set = HSet.(setminus ice useful_set) in
                ice_trim := HSet.(setminus !ice_trim useless_set);
            );
            if not !Cfg.quiet then show_path (translator ice_full !ice_trim) Format.std_formatter [];
        );
        let best = { len=0; path=[]; } in
        (* Phase 1 *)
        if !Cfg.first_pass then (
            bestpath best !ice_trim start single_moves;
            if !Cfg.show_steps then (
                Format.printf "Best path with single moves:\n";
                show_path
                    (translator ice_full ice_full)
                    Format.std_formatter
                    (best.path |> List.rev |> Hex.(path_of_moves start));
            );
        );
        if !Cfg.extremal_pass then (
            (* Phase X *)
            if !Cfg.show_steps then Format.printf "Switching to extremal moves\n";
            bestpath best !ice_trim start extremal_moves;
            if !Cfg.show_steps then (
                Format.printf "Best path with extremal moves:\n";
                show_path
                    (translator ice_full ice_full)
                    Format.std_formatter
                    (best.path |> List.rev |> Hex.(path_of_moves start));
            );
        );
        (* Final phase *)
        if !Cfg.show_steps then Format.printf "Switching to arbitrary moves\n";
        bestpath best !ice_trim start all_moves;
        (best.len, List.rev best.path)
end
            
