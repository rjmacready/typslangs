type term =
		Var of int
	| Abs of term
	| App of term*term


let rec printtm ctx t = match t with
		Abs(e) -> String.concat "" ["lambda . "; printtm ctx e]
	| App(l, r) ->
		String.concat "" ["("; printtm ctx l; " "; printtm ctx r; ")"]
	| Var(i) ->
		string_of_int i;;

type context = (string  * binding) list


Printf.printf "hi\n";
Printf.printf "%s\n" (printtm 0 (Abs (Var 0)));

