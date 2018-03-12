open Multiset

val distance_manhattan : float list -> float list -> float
val distance_euclidienne : float list -> float list -> float
val l_points_average : 'a -> float list bst list -> float list list
val partition_ini : 'a bst -> int -> 'a list * 'b bst list
val partition_bst :
  ('a -> float list -> float) ->
  float list bst ->
  'a list * float list bst list ->
  float list list * float list bst list * float
val k_moyenne :
  (float list -> float list -> float) ->
  float list bst -> int -> 'a -> float list bst list
val color_l_bst : 'a list -> 'b list -> ('a * 'b) list
