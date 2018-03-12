(*Multiset*)

(*Structure récursive du multi-ensemble sous forme d'un arbre binaire de recherche*)
type 'a bst = Empty |
	Cons of (('a * int) *	 'a bst * 'a bst)
;;

(*Fonction d'ajout d'un élèment z en queue de la liste l*)
let add_in_list = fun e l ->
	List.rev (e::(List.rev l))
;;

(*Création d'un ABR vide*)
let create_bst_empty = Empty;;

(*Test de vacuité d'un ABR*)
let bst_is_empty bst = (bst = Empty);;

(*Fonction d'ajout d'un élèment dans l'ABR*)
let rec add = fun elem bst ->
  match bst with
  	| Empty -> Cons ((elem, 1), Empty, Empty)
  	| Cons ((e, w), bstl, bstr) when e = elem ->
    		Cons((e, (w + 1)), bstl, bstr)
  	| Cons ((e, w), bstl, bstr) when e > elem ->
        Cons ((e, w), (add elem bstl), bstr)
  	| Cons ((e, w), bstl, bstr) ->
        Cons ((e, w), bstl, (add elem bstr))
;;

(*Fonctions de suppression d'un élèment dans l'ABR par remontée gauche*)
let rec remove_elem_max = function bst ->
  match bst with
		| Cons ((e, w), bstl, Empty) ->
				(bstl, (e, w))
  	| Cons ((e, w), bstl, bstr) ->
			let (new_bstr, max) = (remove_elem_max bstr)
			in (Cons ((e, w), bstl, new_bstr), max)
		| _ -> failwith "remove_elem_max: l'ensemble est vide."
;;

let remove_elem = function bst ->
  match bst with
  	| Cons ((e, w), bstl, bstr) when w > 1->
				Cons ((e, (w - 1)), bstl, bstr)
		| Cons ((e, w), Empty, bstr) -> bstr
  	| Cons ((e, w), bstl, bstr) ->
    		let (new_bstl,new_node) = (remove_elem_max bstl)
    		in Cons (new_node, new_bstl, bstr)
		| _ -> failwith "remove_elem: binary search tree is empty."
;;

let rec remove = fun elem bst ->
  match bst with
  | Empty -> Empty
  | Cons ((e, w), bstl, bstr) when e = elem ->
      (remove_elem bst)
  | Cons ((e, w), bstl, bstr) when e > elem ->
      Cons ((e, w), (remove elem bstl), bstr)
  | Cons ((e, w), bstl, bstr) ->
      Cons ((e, w), bstl, (remove elem bstr))
;;

(*Création d'un ABR à partir d'une liste d'élèment*)
let create_bst = function l ->
	let rec aux = fun l acc ->
		match l with
		| [] -> acc
		| ee::ll -> (aux ll (add ee acc))
	in (aux l create_bst_empty)
;;

(*Fonctions donnant une liste de n nombres aléatoires, compris entre min et max, aux dimensions définis*)
let random_aux = fun min max ->
    let rec aux = fun min max acc ->
      match (min, max) with
      | (emin::llmin, emax::llmax) ->
        if (emin > emax) then
          failwith "random_aux: max doit être plus grand ou égal que min"
        else (aux llmin llmax (add_in_list (emin +. (Random.float (emax -. emin))) acc))
      | _ -> acc
    in (aux min max [])
;;

let random = fun min max n ->
    if (n < 0) then
      failwith "random: n doit être plus grand ou égal que 0"
    else if (min > max) then
      failwith "random: max doit être plus grand ou égal que min"
    else
			let rec aux = fun min max n acc ->
        match n with
        | 0 -> acc
        | n -> (aux min max (n - 1) (add_in_list (random_aux min max) acc))
      in (aux min max n [])
;;

(*Création d'un ABR de n élèments aléatoires, compris min et max, aux dimensions définis*)
let create_bst_random = fun min max n ->
	let l = (random min max n)
	in (create_bst l)
;;

(*Fonction d'obtention de l'élèment en racine de l'ABR*)
let elem_root = function bst ->
	match bst with
	| Empty -> failwith "elem_root: multiset is empty"
	| Cons ((e, _), _, _) -> e
;;

(*Fonction d'obtention du sous ABR selon la direction*)
type direction = Left | Right;;

let next = fun bst direction ->
	match bst with
	| Empty -> failwith "next: multiset is empty"
	| Cons (_, bstl, bstr) ->
		if (direction = Left) then bstl
		else bstr
;;
