(*Appel générique*)
open Graphics

open Multiset
open Kmoyenne
open Graphic

(*Fonction de dessin du repère avec sélection*)
type mode = Fixe | Dynamique;;

let repere = fun f bst k l_color mode dim_x dim_y weight_rank weight_p ->
	if (mode = Fixe) then (repere_fixe f bst k l_color dim_x dim_y weight_rank weight_p)
	else (repere_dyna f bst k l_color dim_x dim_y weight_rank weight_p)
;;


(*Exemple fixe*)
repere distance_euclidienne (create_bst_random [-25.;-25.] [25.;25.] 20) 3 [blue;red;green] Fixe 1280 720 10 5;

(*Exemple dynamique*)
repere distance_euclidienne (create_bst_random [-25.;-25.] [25.;25.] 20) 3 [blue;red;green] Dynamique 1280 720 10 5;

(*Exemple 5 nuages*)
repere distance_euclidienne (create_bst_random [-25.;-25.] [25.;25.] 50) 5 [blue;red;green;black;cyan] Dynamique 1280 720 10 5;

(*Exemple positif*)
repere distance_euclidienne (create_bst_random [5.;5.] [25.;25.] 15) 3 [blue;red;green] Dynamique 1280 720 10 5;

(*Exemple négatif*)
repere distance_euclidienne (create_bst_random [-25.;-25.] [-5.;-5.] 20) 3 [blue;red;green] Dynamique 1280 720 10 5;

(*Exemple distance de Manhattan*)
repere distance_manhattan (create_bst_random [-25.;-25.] [25.;25.] 20) 3 [blue;red;green] Dynamique 1280 720 10 5;

(*Exemple du sujet*)
repere distance_euclidienne (create_bst [[1.;2.];[1.;1.];[3.;6.]]) 2 [blue;red] Dynamique 1280 720 10 5;;
