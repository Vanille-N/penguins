(** Compute the optimal path *)

(** Generic interface to a grid *)
module type S = sig
    val grid : bool Hex.grid
end

(** Generate calculator from grid *)
module Make (M : S) : sig

    (** Set of all available positions *)
    module HSet : Bitset.SET with type elt = Hex.pos

    (** {2 Util} *)

    (** Print a path

        Successive positions indicated by [a ... z A .. Z 0 ... 9 ? ... ],
        and [*] for unused positions. *)
    val pp_path : Format.formatter -> Hex.pos list -> unit

    (** List of all allowed moves given a configuration

        See [README.pdf] for details on variations of this
        function *)
    val all_moves : HSet.t -> Hex.pos -> Hex.move list

    (** {2 Graph exploration} *)

    (** Connected component from the initial position *)
    val accessible : HSet.t -> HSet.elt -> HSet.t

    (** [disconnected set elt] determines if [set] is split into
        several connected components by the removal of [elt].

        It makes no assumption as to whether or not [elt] belongs
        to [set] before being removed, but it does assume that
        [add set elt] is connected.

        @deprecated You might as well check directly the length of [accessible] *)
    val disconnected : HSet.t -> HSet.elt -> bool

    (** Recalculate the list of connected components

        Actual implementation does not perform the full graph
        exploration if [disconnected set elt = false] *)
    val split : HSet.t -> Hex.pos -> HSet.t list

    (** {2 Main computation} *)

    (** Compute the optimal path and return its length *)
    val maxpath : Hex.pos -> int * Hex.move list

end
