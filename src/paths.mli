module type S = sig
  val grid : bool Hex.grid
end

module Make (M : S) : sig

  (** Ensembles de cases servant pour les configurations *)
  module HSet : Bitset.SET with type elt = Hex.pos

  (** Affichage d'un chemin sous la forme d'une liste
    * de positions. Le chemin devra apparaitre, sur la grille
    * [M.grid] où les cases de glace sont représentées par '*',
    * selon une numérotation des cases du chemin par
    *   a ... z A ... Z 0 ... 9 puis ? pour les éventuelles
    *                                cases suivantes. *)
  val pp_path : Format.formatter -> Hex.pos list -> unit

  (** Liste de tous les mouvements faisables sur la configuration
    * donnée par un ensemble et une position (non présente dans
    * l'ensemble). *)
  val all_moves : HSet.t -> Hex.pos -> Hex.move list

  (** [accessible set elt] renvoie le sous-ensemble
    * correspondant à la composante connexe de [elt]. *)
  val accessible : HSet.t -> HSet.elt -> HSet.t

  (** [disconnected set elt] détermine si [set] a été déconnecté
    * par la suppression de [elt]: en supposant que [elt] n'est
    * pas dans [set] et que [HSet.add set elt] était connexe,
    * il s'agit de déterminer si [set] est connexe. *)
  val disconnected : HSet.t -> HSet.elt -> bool

  (** Si [set] a été déconnecté par la suppression de [elt],
    * renvoie la liste des nouvelles composantes connexes. *)
  val split : HSet.t -> Hex.pos -> HSet.t list

  (** Calcul de la solution optimale à partir d'une position
    * donnée. L'entier est simplement la longueur de la liste
    * de mouvements à effectuer. *)
  val maxpath : Hex.pos -> int * Hex.move list

end
