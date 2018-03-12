open Graphics

open Multiset
open Kmoyenne

val repere_fixe :
  (float list -> float list -> float) ->
  float list bst ->
  int -> Graphics.color list -> int -> int -> int -> int -> unit
val repere_dyna :
  (float list -> float list -> float) ->
  float list bst ->
  int -> Graphics.color list -> int -> int -> int -> int -> unit
