(* Graphique *)
open Graphics

open Multiset
open Kmoyenne

let min = fun e1 e2 ->
	if (e1 < e2) then e1 else e2
;;

let max = fun e1 e2 ->
	if (e1 > e2) then e1 else e2
;;

(*Fonction d'attente*)
let rec wait = function time ->
	match time with
	| 0 -> ()
	| _ -> (wait (time - 1))
;;

(*Fonction de recherche d'une extrémité selon la fonction f dans les x d'un ABR*)
let extremite_x_bst = fun f bst d ->
	let rec aux = fun bst acc ->
		match bst with
		|	Empty -> acc
		| Cons (((e::_), _), _, _) when (f e acc) = e -> (aux (next bst d) e)
		| _ -> (aux (next bst d) acc)
	in
		if ((bst_is_empty bst) = true)
		then failwith "extremite_x_bst : impossible de trouver une extrémité d'un ensemble vide"
		else
			let e = (elem_root bst)
			and bstn = (next bst d)
			in (aux bstn (List.nth e 0))
;;

(*Fonction de recherche d'une extrémité selon la fonction f dans les y d'un ABR*)
let rec extremite_y_bst = fun f bst d ->
	match bst with
	| Empty -> failwith "extremite_y_bst : impossible de trouver une extrémité d'un ensemble vide"
	| Cons (((_::e::_),_), Empty, Empty) -> e
	| Cons (((_::e::_),_), bstl, Empty) -> (f e (extremite_y_bst f bstl d))
	| Cons (((_::e::_),_), Empty, bstr) -> (f e (extremite_y_bst f bstr d))
	| Cons (((_::e::_),_), bstl, bstr) -> (f (f e (extremite_y_bst f bstl d)) (extremite_y_bst f bstr d))
	| _ -> failwith "extremite_y_bst : erreur"
;;

(*Fonction de recherche selon la fonction f_xy et f d'une liste*)
let extremite_l_bst = fun f_xy f l_bst d ->
	let rec aux = fun l_bst acc ->
		match l_bst with
		| [] ->  acc
		| (bst::ll_bst) -> (aux ll_bst (f (f_xy f bst d) acc))
	in
		if (l_bst = []) then failwith "extremite_l_bst : la liste d'ABR est vide"
		else (aux l_bst (f_xy f (List.nth l_bst 0) d))
;;

(*Fonction de définition du minimum et du maximum*)
let define_min_max = fun f l_bst ->
	let min = (int_of_float (floor (extremite_l_bst f min l_bst Left))) - 1
	and max = (int_of_float (ceil (extremite_l_bst f max l_bst Right))) + 1
	in
		if (min > 0 && max > 0) then (0, max)
		else if (min < 0 && max < 0) then (min, 0)
		else (min, max)
;;

(*Fonction de dessin d'un rang de l'axe des abscisses*)
let trace_rang_x = fun weight_rank p y ->
	(moveto p y);
	(lineto p (y + (weight_rank / 2)));
	(lineto p (y - (weight_rank / 2)))
;;

(*Fonction de dessin d'un rang de l'axe des ordonnées*)
let trace_rang_y = fun weight_rank p x ->
	(moveto x p);
	(lineto (x + (weight_rank / 2)) p);
	(lineto (x - (weight_rank / 2)) p)
;;

(*Fonction de dessin des rangs*)
let trace_rang = fun f coord dim d weight_rank ->
	let rec aux = fun p ->
		match p with
 		| p when (p > (dim - d)) ->
			(f weight_rank p coord);
		| p ->
			(f weight_rank p coord);
			(aux (p + d))
	in (aux 0)
;;

(*Fonction de dessin de l'axe des abscisses*)
let trace_axe_x = fun y dim_x->
	(moveto 0 y);
	(lineto dim_x y)
;;

(*Fonction de dessin de l'axe des ordonnées*)
let trace_axe_y = fun x dim_y->
	(moveto x 0);
	(lineto x dim_y)
;;

(*Fonction de dessin des axes*)
let trace_axe = fun trace min dim dim2 d ->
	let rec aux = fun p min ->
		match min with
		| min when ((min >= 0) || (p > (dim2 - d))) ->
  			(trace p dim);
  			p
		| min ->
				(aux (p + d) (min + 1))
	in (aux 0 min)
;;

(*Fonction de dessin des axes et des rangs*)
let trace_xy = fun min_x min_y dim_x dim_y d_betw_p_x d_betw_p_y weight_rank ->
	let axe_x = (trace_axe trace_axe_x min_y dim_x dim_y d_betw_p_y)
	and axe_y = (trace_axe trace_axe_y min_x dim_y dim_x d_betw_p_x)
	in 	(trace_rang trace_rang_x axe_x dim_x d_betw_p_x weight_rank);
			(trace_rang trace_rang_y axe_y dim_y d_betw_p_y weight_rank);
			(axe_x, axe_y)
;;

(*Fonction de dessin d'un point*)
let trace_point = fun e axe_x axe_y d_betw_p_x d_betw_p_y weight_p color->
	let x = (int_of_float ((float_of_int axe_y) +. (List.nth e 0) *. (float_of_int (d_betw_p_x))))
	and y = (int_of_float ((float_of_int axe_x) +. (List.nth e 1)	*. (float_of_int (d_betw_p_y))))
	in set_color color;
		(wait 2000000);
		(fill_circle x y weight_p)
;;

(*Fonction de dessin d'un ABR*)
let trace_bst = fun bst axe_x axe_y d_betw_p_x d_betw_p_y weight_p color->
	let rec aux = function bst ->
		match bst with
  	| Empty -> failwith "trace_bst : erreur l'ABR est vide."
		| Cons ((ee, _), Empty, Empty) ->
			(trace_point ee axe_x axe_y d_betw_p_x d_betw_p_y weight_p color)
		| new_bst ->
			let e = (elem_root new_bst)
			in (trace_point e axe_x axe_y d_betw_p_x d_betw_p_y weight_p color);
				(aux (remove e new_bst))
	in (aux bst)
;;

(*Fonction de dessin d'une liste ABR*)
let trace_l_bst = fun l_bst axe_x axe_y d_betw_p_x d_betw_p_y weight_p ->
	let rec aux = function l_bst ->
		match l_bst with
		| [] -> failwith "trace_l_bst : erreur la liste d'ABR est vide."
		| (bst, color)::[] ->
				(trace_bst bst axe_x axe_y d_betw_p_x d_betw_p_y weight_p color)
		| (bst, color)::ll_bst ->
				(trace_bst bst axe_x axe_y d_betw_p_x d_betw_p_y weight_p color);
				(aux ll_bst)
	in (aux l_bst)
;;

(*Partie fixe*)

(*Fonction de dessin du repère et des partitions*)
let graph_repere_fixe = fun l_bst l_color dim_x dim_y weight_rank weight_p ->
	let (min_x, max_x) = (define_min_max extremite_x_bst l_bst)
	and (min_y, max_y)=  (define_min_max extremite_y_bst l_bst)
	in
  	let d_x = (abs (max_x - min_x))
  	and d_y = (abs (max_y - min_y))
  	in
    	let d_betw_p_x = ((dim_x) / d_x)
    	and d_betw_p_y = ((dim_y) / d_y)
    	in
    		open_graph (" "^(string_of_int dim_x)^"x"^(string_of_int dim_y));
    		let (axe_x, axe_y) = (trace_xy min_x min_y dim_x dim_y d_betw_p_x d_betw_p_y weight_rank)
    		in
					(trace_l_bst (color_l_bst l_bst l_color) axe_x axe_y d_betw_p_x d_betw_p_y weight_p);
					(wait 150000000);
					close_graph ()
;;

(*Fonction de dessin du repère fixe*)
let repere_fixe = fun f bst k l_color dim_x dim_y weight_rank weight_p->
	let l_bst =	(k_moyenne f bst k l_color)
	in (graph_repere_fixe l_bst l_color dim_x dim_y weight_rank weight_p)
;;

(*Partie dynamique*)

(*Fonction d'obtention de la nouvelle liste d'ABR et de son dessin en mode dynamique*)
let rec partition_dyna = fun f axe_x axe_y d_betw_p_x d_betw_p_y weight_p l_color (l_p_a, l_bst, somme_d_l_bst) ->
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
			if (new_somme_d_l_bst >= somme_d_l_bst) then
        begin
          (wait 120000000);
          close_graph ();
        end
  		else
				begin
          (wait 50000000);
          (trace_l_bst (color_l_bst new_l_bst l_color) axe_x axe_y d_betw_p_x d_betw_p_y weight_p);
          (partition_dyna f axe_x axe_y d_betw_p_x d_betw_p_y weight_p l_color (new_l_p_a, new_l_bst, new_somme_d_l_bst))
				end
;;

(*Fonction de dessin du repère et des partitions en mode dynamique*)
let graph_repere_dyna = fun f l_color dim_x dim_y weight_rank weight_p (l_p_a, l_bst, somme_d_l_bst) ->
	let (min_x, max_x) = (define_min_max extremite_x_bst l_bst)
	and (min_y, max_y)=  (define_min_max extremite_y_bst l_bst)
	in
		let d_x = (abs (max_x - min_x))
		and d_y = (abs (max_y - min_y))
		in
			let d_betw_p_x = ((dim_x) / d_x)
			and d_betw_p_y = ((dim_y) / d_y)
			in
				open_graph (" "^(string_of_int dim_x)^"x"^(string_of_int dim_y));
				let (axe_x, axe_y) = (trace_xy min_x min_y dim_x dim_y d_betw_p_x d_betw_p_y weight_rank)
				in
					(trace_l_bst (color_l_bst l_bst l_color) axe_x axe_y d_betw_p_x d_betw_p_y weight_p);
					(partition_dyna f axe_x axe_y d_betw_p_x d_betw_p_y weight_p l_color (l_p_a, l_bst, somme_d_l_bst))
;;

(*Fonction de dessin du repère dynamique*)
let repere_dyna = fun f bst k l_color dim_x dim_y weight_rank weight_p ->
	(graph_repere_dyna f l_color dim_x dim_y weight_rank weight_p (partition_bst f bst (partition_ini bst k)))
;;
