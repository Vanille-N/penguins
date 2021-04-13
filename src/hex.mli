(** Abstraction over the hexagonal grid *)

type pos = int * int
type 'a grid = 'a array array

(** Directions *)

type dir

(** List of all possible directions *)
val all_directions : dir list

(** Gives all (2) directions that are considered adjacent *)
val neighbors : dir -> dir list

(** Gives the direction considered opposite *)
val opposite : dir -> dir

(** Movements *)

type move = dir * int
val move : pos -> dir -> pos
val move_n : pos -> move -> pos

(** [path_of_moves p moves] is the list of positions
 * obtained by iterating [move] on [moves] starting
 * from [p] *)
val path_of_moves : pos -> move list -> pos list

(** I/O *)

(* util *)
val to_string : dir -> string

(** Print grid of characters with hexagonal formatting *)
val pp_grid : Format.formatter -> char grid -> unit

(** Read (and pad with empty positions) a grid
 * from an input channel *)
val from_channel : in_channel -> pos * bool grid
val show_problem : in_channel -> pos * char grid
