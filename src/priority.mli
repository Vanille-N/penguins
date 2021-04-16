(** A min-heap-based priority queue *)

(** Type of keys *)
module type ORDERED = sig
    type t
    val compare : t -> t -> int
end

(** Priority queue implementation *)
module Make (M:ORDERED) : sig

    (** Priority queue with keys in {!M.t} and values in ['a] *)
    type 'a queue

    (** Node of the queue (possibly deleted since) *)
    type 'a node

    (** {2 Standard queue operations} *) 

    (** [create max_size dummy_key dummy_value] build a new 
      * priority queue with capacity [max_size]. *)
    val create : int -> M.t -> 'a -> 'a queue

    (** Current number of nodes in the queue *)
    val size : 'a queue -> int

    (** Create a node, insert it into the queue, and return it for future access *)
    val insert : 'a queue -> M.t -> 'a -> 'a node

    (** Extract and remove the node with smallest key

        Fails if queue is empty *)
    val extract_min : 'a queue -> 'a node

    (** {2 Node manipulation} *)

    val key : 'a node -> M.t
    val value : 'a node -> 'a

    (** Remove an element from the queue before
        its scheduled priority *)
    val remove : 'a queue -> 'a node -> unit

    (** Search presence of a node in the queue *)
    val member : 'a queue -> 'a node -> bool

    (** Increase the priority of a node, insert it into
        the queue if it does not already exist *)
    val decrease_key : 'a queue -> 'a node -> M.t -> unit

end
