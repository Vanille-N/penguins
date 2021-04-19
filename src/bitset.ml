(* copy signatures from mli *)
module type FIN = sig
    type t
    val max : int
    val to_int : t -> int
    val of_int : int -> t
end

module type SET = sig
  type t
  type elt

  val cardinal : t -> int
  val empty : t
  val init : (elt -> bool) -> t

  val add : t -> elt -> t
  val remove : t -> elt -> t
  val member : t -> elt -> bool
  val subset : t -> t -> bool

  val iter : t -> (elt -> unit) -> unit
  val setminus : t -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val compare : t -> t -> int

  val transitive_closure : elt -> (elt -> elt list) -> t
end

module Make (F : FIN) : SET with type elt = F.t = struct
    type t = {
        mutable card: int; (* needs to be mutable in-place for *_mut_* operations *)
        contents: Int64.t array;
    }
    type elt = F.t
 
    (* accessor functions *)
    let ( .!{} ) s k = s.contents.(k)
    let ( .!{}<- ) s k v = s.contents.(k) <- v
    let accessor i = (i / 64, Int64.(shift_left Int64.one (i mod 64)))
    let size = fst (accessor F.max) + 1

    (* easy + util *)
    let empty = { card=0; contents=Array.make size Int64.zero; } 
    let cardinal set = set.card
    let clone set = { card=set.card; contents=Array.copy set.contents; }

    (* init from indicator function *)
    let init indic =
        let set = clone empty in
        for i = 0 to F.max-1 do
            if indic (F.of_int i) then (
                (* add to set (in-place) *)
                set.card <- set.card + 1;
                let (n, k) = accessor i in
                set.!{n} <- Int64.(logor set.!{n} k)
            )
        done;
        set

    (* functions that manipulate bits for internal use:
     * the exposed interface uses [F.t] but [(int*int)] has
     * better performance because fewer conversions *)
    let member_bit set (n,k) =
        not Int64.(equal (logand set.!{n} k) Int64.zero)
    
    (* these functions mutate the set
     * the interface however is immutable *)
    let add_bit_mut_unchecked set (n,k) =
        set.!{n} <- Int64.(logor set.!{n} k);
        set.card <- set.card + 1

    let add_bit_mut set (n,k) =
        if not (member_bit set (n,k)) then (
            add_bit_mut_unchecked set (n,k)
        )

    let remove_bit_mut_unchecked set (n,k) =
        set.!{n} <- Int64.(logand set.!{n} (lognot k));
        set.card <- set.card - 1 

    let remove_bit_mut set (n,k) =
        if member_bit set (n,k) then (
            remove_bit_mut_unchecked set (n,k)
        )

    (* would be easy to express in terms of [member_bit],
     * [add_bit_mut], [remove_bit_mut] respectively,
     * but let's use unchecked versions instead to have fewer
     * copies *)
    let member set elem =
        member_bit set (accessor (F.to_int elem))

    let add set elem =
        let acc = accessor (F.to_int elem) in
        if member_bit set acc then (
            set (* no-op *)
        ) else (
            (* modify a copy *)
            let set = clone set in
            add_bit_mut_unchecked set acc;
            set
        )

    let remove set elem =
        let acc = accessor (F.to_int elem) in
        if member_bit set acc then (
            (* modify a copy *)
            let set = clone set in
            remove_bit_mut_unchecked set acc;
            set
        ) else (
            set (* no-op *)
        )

    (* faster than loop over 0..max because checks 64 bits at a time
     * (also short-circuits) *)
    let subset lt rt =
        let rec aux i =
            i = size
            || ((Int64.(equal rt.!{i} (logor lt.!{i} rt.!{i}))) && (aux (i+1)))
        in aux 0

    let foreach set (fn:(int*Int64.t)->unit) =
        for i = 0 to F.max-1 do
            let acc = accessor i in
            if member_bit set acc then
                fn acc
        done

    let iter set fn =
        for i = 0 to F.max-1 do
            if member_bit set (accessor i) then
                fn (F.of_int i)
        done

    let collect set =
        let rec aux acc = function
            | (-1,_) -> acc
            | (n,k) -> (
                let rest acc =
                    if k = 0
                    then aux acc (n-1, 63)
                    else aux acc (n, k-1)
                in
                if member_bit set (n, Int64.(shift_left one k))
                then rest (F.of_int (n * 64 + k) :: acc)
                else rest acc
            )
        in aux [] (F.max / 64, F.max mod 64)

    (* faster than loop because bypasses [to_int] and [of_int] conversions
     * also avoid copies *)
    let setminus set set' =
        let set = clone set in
        foreach set' (remove_bit_mut set);
        set

    let union set set' =
        let set = clone set in
        foreach set' (add_bit_mut set);
        set

    let intersect set set' =
        init (fun e -> let i = F.to_int e in member_bit set (accessor i) && member_bit set' (accessor i))

    (* sort by increasing cardinal *)
    let compare s s' =
        compare (cardinal s') (cardinal s)

    (* DFS exploration *)
    let transitive_closure start near =
        (* use mutability for performance *)
        let seen = clone empty in
        let rec explore = function
            | [] -> seen
            | pos :: rest when not (member_bit seen (accessor pos)) ->
                add_bit_mut seen (accessor pos); 
                let adj = near (F.of_int pos)
                    |> List.map F.to_int
                    |> List.filter (fun p -> not (member_bit seen (accessor p)))
                in
                explore (adj @ rest)
            | _ :: rest -> explore rest
        in explore [F.to_int start]
end
        
