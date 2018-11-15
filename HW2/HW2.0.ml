type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*
input: a rule list, a non-terminal value
output: the alternative list of the nt value
*)
let rec get_alter_list rule_list nt_val=
	match rule_list with
	| [] -> []
	| (value,rhs)::rest ->
		if value = nt_val
			then rhs::(get_alter_list rest nt_val)
			else get_alter_list rest nt_val
(*
input: hw1-style grammar, (start symbol value, rule_list)
output: hw2-style grammar, (start symbol value, production function)
*)
let convert_grammar (start_val,rule_list) =
	start_val, get_alter_list rule_list

(*
pf: production function
accept: acceptor
d: derivation
frag: fragment
 *)

let nt_match nt_val pf alter_ls_match rhs_match d frag =
  let alter_ls = pf nt_val in
  match alter_ls with
  | [] -> Some ( d@[(nt_val,[])], frag )
  | _ -> alter_ls_match nt_val alter_ls rhs_match pf d frag 

(*
input: a non-terminal value, its alternative list, rhs_match function,
	   derivation, fragment
output:
	None if no alternative in the list matches frag
	Some (derivation@new rule, suffix) if some alternative matches a prefix of frag
*)
let rec alter_ls_match nt_val alter_ls rhs_match pf d frag =
  match alter_ls with
  | [] -> None
  | rhs::rest ->
  	let try_rhs = rhs_match rhs pf (d@[(nt_val,rhs)]) frag in
  	match try_rhs with
  	| None -> alter_ls_match nt_val rest rhs_match pf d frag
  	| Some (d',suffix) -> try_rhs (*!!!!!*)

let t_match t_val frag =
  match frag with
  | [] -> None
  | hd::suffix ->
     if hd = t_val then Some suffix else None

(*
input: symbol list, derivation(containing current rule), fragment
output:
	None if the rhs of the rule does not match fragment
	Some (derivation', suffix) if the rhs match a prefix of frag
*)

let rec rhs_match symbol_ls pf d frag =
  match symbol_ls with
  | [] -> Some (d, frag)
  | s::rest ->
     match s with
     | T t_val ->
        (match t_match t_val frag with
        | None -> None
        | Some suffix -> rhs_match rest pf d suffix)
     | N nt_val ->
     	(match nt_match nt_val pf alter_ls_match rhs_match d frag with
     	| None -> None
     	| Some (d',suffix) -> rhs_match rest pf d' suffix)

