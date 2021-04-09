(* should print debug info at each step *)
val debug : bool ref

(* should print the current state of the grid at each step *)
val display : bool ref

(* allow special formatting characters *)
val ansi_fmt : bool ref

(* utilities *)
val any : ('a -> bool) -> 'a list -> bool
val all : ('a -> bool) -> 'a list -> bool

(* useful for debug and general-purpose printing *)
val inspect : ?b:bool -> ('a -> unit) -> 'a list -> 'a list
val passthrough : ?b:bool -> ('a -> unit) -> 'a -> 'a 
