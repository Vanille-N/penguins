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

