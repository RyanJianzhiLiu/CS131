(*
input: a rule list, a non-terminal value
output: the alternative list of the nt value
*)
let rec get_alter_list rule_list nt_val=
	match rule_list with
	| [] -> []
	| (value,rhs)::rest ->
		if value = nt_val
			then rhs::(get_alter_list nt_val rest)
			else get_alter_list nt_val rest
(*
input: hw1-style grammar, (start symbol value, rule_list)
output: hw2-style grammar, (start symbol value, production function)
*)
let convert_grammar gram1 =
	match gram1 with
	| (start_val,rule_list) -> (start_val, get_alter_list rule_list)


(*
input: a terminal value t, a optional fragment
output: Some suffix if head of fragment matches t
		None otherwise
*)
let terminal_match t_val opt_frag =
	match opt_frag with
	| None -> None
	| Some frag ->
		match frag with
		| [] -> None
		| hd::rest ->
		   if hd = t_val then Some rest else None


(*
pf: production function
accept: acceptor
d: derivation
frag: fragment
 *)
                  
let rec nt_match nt_val pf d frag =
  let alter_ls = pf nt_val in
  match alter_ls with
  | [] -> Some ( d@(nt_val,[]), frag )
  | _ -> alter_ls_match alter_ls



let rec alter_ls_match nt_val alter_ls d frag =
  match alter_ls with
  | [] -> None
  | rhs::rest_alter ->
     match rhs with
     | [] -> Some ( d@(nt_val,[]), frag )
     | _ ->
        let try_rhs = rhs_match rhs in
        match try_rhs with
        | None -> alter_ls_match rest_alter accept d frag
        | Some (d',frag') -> None

                                              
let rec rhs_match lhs rhs d frag =
  match rhs with
  | [] -> Some frag
  | s::rest ->
     match s with
     | T t_val ->
        match t_match t_val frag with
        | None -> None
        | Some frag' -> rhs_match
                     
     | N nt_val -> None

let t_match t_val accept frag =
  match frag with
  | [] -> None
  | hd::rest ->
     if hd = t_val then accept rest else None

(*
let match_n accept frag =
    match frag with
    | [] -> None
    | h::t -> if h = n then accept t else None

let append_m m1 m2 accept f =
    m1 (m2 accept) f

To append m1 m2 m3
m = append_m m1 m2 accept f
append_m m (match_n 5)
*)
