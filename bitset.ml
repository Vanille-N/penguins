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
