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
        mutable card: int;
        contents: Int64.t array;
    }
    type elt = F.t
 
    (* accessor functions *)
    let ( .!{} ) s k = s.contents.(k)
    let ( .!{}<- ) s k v = s.contents.(k) <- v
    let one = Int64.of_int 1
    let accessor i = (i / 64, Int64.(shift_left one (i mod 64)))
    let size = fst (accessor F.max) + 1

    (* easy + util *)
    let empty = { card=0; contents=Array.make size (Int64.of_int 0); } 
    let cardinal set = set.card
    let clone set = { card=set.card; contents=Array.copy set.contents; }

    (* init from indicator function *)
    let init indic =
        let set = clone empty in
        for i = 0 to F.max-1 do
            if indic (F.of_int i) then (
                set.card <- set.card + 1;
                let (n, k) = accessor i in
                set.!{n} <- Int64.(logor set.!{n} k)
            )
        done;
        set

    (* functions that manipulate bits for internal use:
     * the exposed interface uses [F.t] but [int] has
     * better performance *)
    let member_bit set i =
        let (n, k) = accessor i in
        not Int64.(equal (logand set.!{n} k) (of_int 0))
    
    (* these functions mutate the set
     * the interface however is immutable *)
    let add_bit_mut set i =
        let (n, k) = accessor i in
        if not (member_bit set i) then (
            set.!{n} <- Int64.(logor set.!{n} k);
            set.card <- set.card + 1
        )

    let remove_bit_mut set i =
        let (n, k) = accessor i in
        if member_bit set i then (
            set.!{n} <- Int64.(logand set.!{n} (lognot k));
            set.card <- set.card - 1
        )

    (* easy to express in terms of [member_bit],
     * [add_bit_mut], [remove_bit_mut] respectively *)
    let member t elem =
        member_bit t (F.to_int elem)

    let add t elem =
        let t = clone t in
        add_bit_mut t (F.to_int elem);
        t

    let remove t elem =
        let t = clone t in
        remove_bit_mut t (F.to_int elem);
        t

    (* faster than loop because checks 64 bits at a time
     * (also short-circuits) *)
    let subset lt rt =
        let rec aux i =
            i = size
            || ((Int64.(equal rt.!{i} (logor lt.!{i} rt.!{i}))) && (aux (i+1)))
        in aux 0

    let foreach set (fn:int->unit) =
        for i = 0 to F.max-1 do
            if member_bit set i then
                fn i
        done

    let iter set fn =
        foreach set (fun i -> fn (F.of_int i))

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
        init (fun e -> let i = F.to_int e in member_bit set i && member_bit set' i)

    (* sort by increasing cardinal *)
    let compare s s' =
        compare (cardinal s') (cardinal s)

end
        
