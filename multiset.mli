type 'a bst = Empty | Cons of (('a * int) * 'a bst * 'a bst)
val add_in_list : 'a -> 'a list -> 'a list
val create_bst_empty : 'a bst
val bst_is_empty : 'a bst -> bool
val add : 'a -> 'a bst -> 'a bst
val remove : 'a -> 'a bst -> 'a bst
val create_bst : 'a list -> 'a bst
val create_bst_random : float list -> float list -> int -> float list bst
val elem_root : 'a bst -> 'a
type direction = Left | Right
val next : 'a bst -> direction -> 'a bst
