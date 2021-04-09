
type pos = int * int
type 'a grid = 'a array array

(** Directions *)

type dir

(** Liste de toutes les directions possibles *)
val all_directions : dir list

(* Gives all (2) directions that are considered "adjacent" *)
val neighbors : dir -> dir list

(* Gives the direction considered "opposite" *)
val opposite : dir -> dir

(** Mouvements *)

type move = dir * int
val move : pos -> dir -> pos
val move_n : pos -> move -> pos

(** [path_of_moves p moves] est la liste des positions
  * obtenues par applications successives des mouvements
  * de [moves] à partir de [p]. *)
val path_of_moves : pos -> move list -> pos list

(** Entrées/sorties *)

(* util *)
val to_string : dir -> string

(** Affichage d'une grille de caractères. *)
val pp_grid : Format.formatter -> char grid -> unit

(** Lecture d'une grille sur un in_channel.
  * La grille produite devra toujours avoir un pourtour
  * vide, même si cela n'est pas le cas dans le problème
  * d'entrée. *)
val from_channel : in_channel -> pos * bool grid
val show_problem : in_channel -> pos * char grid
