(** Configuration options *)

(** {2 Appearance} *)

(** Should print debug info at each step *)
val debug : bool ref

(** Should print the current state of the grid at each step *)
val display : bool ref

(** Allow special formatting characters *)
val ansi_fmt : bool ref

(** Print nothing *)
val quiet : bool ref


(** {2 Performance} *)

(** Start with a restriction to single moves *)
val first_pass : bool ref

(** Follow with a restriction to single and maximal moves *)
val extremal_pass : bool ref

(** Recursively trim useless positions *)
val trim : bool ref

(** {2 Util} *)

(** Useful for debug and general-purpose printing *)

val inspect : ?b:bool -> ('a -> unit) -> 'a list -> 'a list
val passthrough : ?b:bool -> ('a -> unit) -> 'a -> 'a 
