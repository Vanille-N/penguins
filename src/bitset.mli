(** A finite type whose elements can be mapped one-to-one with
 * the integers of [0..max-1] thanks to [to_int] and [of_int]
 *
 * It is permitted that calling [of_int] on a value greater
 * than [max] lead to undefined behavior. *)
module type FIN = sig
  type t
  val max : int
  val to_int : t -> int
  val of_int : int -> t
end

(** A set [t] of elements of type [elt].
 *
 * Set operations expose an immutable interface to an
 * internally mutable object.
 *
 * It is guaranteed that
 * [t = t'] if and only if [subset t t' && subset t' t] *)
module type SET = sig
  type t
  type elt

  (** Cardinal of a set (constant time) *)
  val cardinal : t -> int

  (** Empty set *)
  val empty : t

  (** Initialize set from indicator function *)
  val init : (elt -> bool) -> t

  (** Add element to set *)
  val add : t -> elt -> t

  (** Remove element from set *)
  val remove : t -> elt -> t

  (** Test belonging to set *)
  val member : t -> elt -> bool

  (** Check if first set is subset of the second *)
  val subset : t -> t -> bool

  (** Iterate function on all elements of the set *)
  val iter : t -> (elt -> unit) -> unit
    
  (** Standard set operations *)
  val setminus : t -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t

  (** Comparison function to sort in decreasing order of cardinal *)
  val compare : t -> t -> int

  (** A DFS to easily create a set from a starting point and a neighbor function *)
  val transitive_closure : elt -> (elt -> elt list) -> t
end

(** Implementation of [SET] given some [F:FIN] *)
module Make (F : FIN) : SET with type elt = F.t