(* k-moyenne *)

open Multiset

(*Fonctions de calculs de distance*)
let abs_float = function x ->
	if (x < 0.) then (-1. *. x) else x
;;

let distance_manhattan = fun e1 e2 ->
	let rec aux = fun l1 l2 acc ->
		match (l1, l2) with
		| ([], []) -> acc
		| ((c1::ll1), (c2::ll2)) ->
				(aux ll1 ll2 ((abs_float (c2 -. c1)) +. acc))
		| _ -> failwith "distance_manhattan : la construction des points est fausse"
	in (aux e1 e2 0.)
;;

let distance_euclidienne = fun e1 e2 ->
	let rec aux = fun l1 l2 acc ->
		match (l1, l2) with
		| ([], []) -> acc
		| ((c1::ll1), (c2::ll2)) -> (aux ll1 ll2 (((c2 -. c1) ** 2.) +. acc))
		| _ -> failwith "distance_euclidienne : la construction des points est fausse"
	in (sqrt (aux e1 e2 0.))
;;

(*Fonction d'obtention du point moyen d'un ABR donné*)
let point_average = fun f bst ->
	let rec aux = fun bst k acc ->
		match bst with
		| Empty when (k = 0.) -> failwith "point_average : erreur division par zero"
		| Empty ->
			let acc_x = (List.nth acc 0) and acc_y = (List.nth acc 1)
			in ((acc_x /. k)::(acc_y /.k)::[])
		| bst ->
			let e = (elem_root bst)
			in
				let x = (List.nth e 0) 				and y = (List.nth e 1)
				and acc_x = (List.nth acc 0) 	and acc_y = (List.nth acc 1)
				in (aux (remove e bst) (k +. 1.) ((x +. acc_x)::(y +. acc_y)::[]))
	in (aux bst 0. [0.;0.])
;;

(*Fonction d'obtention de la liste des points moyens d'une liste d'ABR donné*)
let l_points_average = fun f l_bst->
	let rec aux = fun l_bst acc ->
		match l_bst with
		| [] -> acc
		| (bst::ll_bst) -> (aux ll_bst (add_in_list (point_average f bst) acc))
	in (aux l_bst [])
;;

(*Fonction d'initialisation d'un ABR en une liste du nombre de partitions d'ABR vide avec la liste de leurs points moyens*)
let partition_ini = fun bst k ->
	let rec aux = fun bst k l_p_a l_bst ->
		match k with
		| 0 -> (l_p_a, l_bst)
		| k ->
			let e = (elem_root bst)
			in (aux (remove e bst) (k - 1) (add_in_list e l_p_a) (add_in_list create_bst_empty l_bst))
	in (aux bst k [] [])
;;

(*Fonction de recherche de la distance minimale entre un point et la liste des points moyens*)
let d_min_in_list = fun f l_p_a e ->
	let rec aux = fun l_p_a min ->
		match l_p_a with
		| [] -> min
		| (p_a::ll) ->
			let d = (f p_a e)
			in
				if (d < min) then (aux ll d)
				else (aux ll min)
	in (aux (List.tl l_p_a) (f (List.hd l_p_a) e))
;;

(*Fonction d'ajout de l'élèment donné dans la partition la plus proche*)
let add_in_l_bst = fun f (l_p_a, l_bst) e ->
	let d_min = (d_min_in_list f l_p_a e)
	in
		let rec aux = fun (l_p_a, l_bst) acc ->
			match (l_p_a, l_bst) with
			| ([], []) -> (acc, d_min)
			| ((p_a::ll_p_a), (bst::ll_bst)) ->
  				let d = (f p_a e)
  				in
  					if (d = d_min) then (aux (ll_p_a, ll_bst) (add_in_list (add e bst) acc))
  					else  							(aux (ll_p_a, ll_bst) (add_in_list (remove e bst) acc))
			| _ -> failwith "add_in_l_bst : Erreur de construction"
		in (aux (l_p_a, l_bst) [])
;;

(*Fonction d'ajout des élèments d'un ABR donné dans leurs partitions les plus rapprochés*)
let partition_bst = fun f bst (l_p_a, l_bst) ->
	let rec aux = fun bst l_bst somme_d ->
		match bst with
		| Empty -> ((l_points_average f l_bst), l_bst, somme_d)
		| bst ->
			let e = (elem_root bst)
			in
				let (new_l_bst, d) = (add_in_l_bst f (l_p_a, l_bst) e)
				in (aux (remove e bst) new_l_bst (d +. somme_d))
	in (aux bst l_bst 0.)
;;

(*Fonction d'obtention de la nouvelle liste d'ABR selon la liste de ses points moyens*)
let rec partition = fun f (l_p_a, l_bst, somme_d_l_bst) ->
	let rec aux = fun l_bst acc somme_d ->
		match l_bst with
		| [] -> (acc, somme_d)
		| (bst::ll_bst) ->
			let (_, new_l_bst, d) = (partition_bst f bst (l_p_a, acc))
			in (aux ll_bst new_l_bst (d +. somme_d))
	in
		let (new_l_bst, new_somme_d_l_bst) = (aux l_bst l_bst 0.)
		and new_l_p_a = (l_points_average f l_bst)
		in
			if (new_somme_d_l_bst >= somme_d_l_bst) then new_l_bst
			else (partition f (new_l_p_a, new_l_bst, new_somme_d_l_bst))
;;

(*Fonction de formation de k nuages selon un ABR et une fonction de distance*)
let k_moyenne = fun f bst k l_color ->
	let (l_pa_ini, l_bst_ini) = (partition_ini bst k)
	in
		let (l_pa, l_bst, somme_d) = (partition_bst f bst (l_pa_ini, l_bst_ini))
		in (partition f (l_pa, l_bst, somme_d))
;;

(*Fonction de colorisation d'une liste d'ABR selon une liste de couleur de même longueur*)
let color_l_bst = fun l_bst l_color ->
	let rec aux = fun l_bst l_color acc ->
		match l_bst, l_color with
		| [], [] -> acc
		| (bst::ll_bst), (color::ll_color) -> (aux ll_bst ll_color (add_in_list (bst, color) acc))
		| _ -> failwith "color_l_bst : Erreur dans la construction"
	in
		if ((List.length l_bst) != (List.length l_color))
		then failwith "Donnez un nombre de couleurs correspondant aux nombre de nuages"
		else (aux l_bst l_color [])
;;
