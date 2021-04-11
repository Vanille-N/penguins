(** Display configuration *)

(** should print debug info at each step *)
val debug : bool ref
(** should print the current state of the grid at each step *)
val display : bool ref
(** allow special formatting characters *)
val ansi_fmt : bool ref
(** print nothing *)
val quiet : bool ref


(** Optimize configuration *)

val first_pass : bool ref
val extremal_pass : bool ref


(** Useful for debug and general-purpose printing *)
val inspect : ?b:bool -> ('a -> unit) -> 'a list -> 'a list
val passthrough : ?b:bool -> ('a -> unit) -> 'a -> 'a 
