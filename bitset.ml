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
end

module Make (F : FIN) : SET with type elt = F.t = struct
    type t = int * Int64.t array
    type elt = F.t
 
    let accessor i = (i / 64, Int64.(shift_left (of_int 1) (i mod 64)))
    let size = fst (accessor F.max) + 1

    let empty = (0, Array.make size (Int64.of_int 0))
    let cardinal (nb, _) = nb

    let init indic =
        let set = Array.copy (snd empty) in
        let count = ref 0 in
        for i = 0 to F.max-1 do
            if indic (F.of_int i) then (
                incr count;
                let (n, k) = accessor i in
                set.(n) <- Int64.(logor set.(n) k)
            )
        done;
        (!count, set)

    let member_bit (_, set) i =
        let (n, k) = accessor i in
        not Int64.(equal (logand set.(n) k) (of_int 0))
    
    let add_bit (nb, set) i =
        let set = Array.copy set in
        let (n, k) = accessor i in
        if member_bit (nb, set) i then (
            (nb, set)
        ) else (
            set.(n) <- Int64.(logor set.(n) k);
            (nb + 1, set)
        )

    let remove_bit (nb, set) i =
        let set = Array.copy set in
        let (n, k) = accessor i in
        if member_bit (nb, set) i then (
            set.(n) <- Int64.(logand set.(n) (lognot k));
            (nb - 1, set)
        ) else (
            (nb, set)
        )

    let member t elem = member_bit t (F.to_int elem)
    let add t elem = add_bit t (F.to_int elem)
    let remove t elem = remove_bit t (F.to_int elem)

    let subset (_,lt) (_,rt) =
        let rec aux i =
            i = size
            || ((Int64.(equal rt.(i) (logor lt.(i) rt.(i)))) && (aux (i+1)))
        in aux 0


    let iter set fn =
        for i = 0 to F.max-1 do
            if member_bit set i then
                fn (F.of_int i)
        done

    let setminus set set' =
        let set = ref set in
        for i = 0 to F.max-1 do
            if member_bit set' i then
                set := remove_bit !set i
        done;
        !set

    let union set set' =
        let set = ref set in
        for i = 0 to F.max-1 do
            if member_bit set' i then
                set := add_bit !set i
        done;
        !set

    let intersect set set' =
        let s = ref empty in
        for i = 0 to F.max-1 do
            if member_bit set i && member_bit set' i then
                s := add_bit !s i
        done;
        !s

    let compare s s' =
        compare (cardinal s') (cardinal s)
end
        
