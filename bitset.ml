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

