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



(*Given a non-terminal value, production function and the remaining alternatives,
  return a matcher for the nt value with those alternatives.*)
let rec match_nt nt_val pf match_rhs alternatives = 
  match alternatives with
  (*every alternative is tried and no accepted match found*)
  | [] -> (fun accept d frag -> None)
  (*try the first rhs in the alternatives*)
  | rhs::rest_alternatives ->
    (fun accept d frag ->
      (*try to match rhs wtih frag*)
      let try_rhs = (match_rhs pf rhs) accept (d@[(nt_val,rhs)]) frag in
      match try_rhs with
      (*no match for rhs, try rest of alternatives*)
      | None -> (match_nt nt_val pf match_rhs rest_alternatives) accept d frag
      (*match found*)
      | _ -> try_rhs
    )

(*Given production function and a list of symbols,
  return a matcher for the symbols*)
let rec match_rhs pf symbols =
  match symbols with
  (*all symbols have been matched*)
  | [] -> (fun accept d frag -> accept d frag)
  | s::rest_symbols ->
    match s with
    (*if s is non-terminal, use non-terminal matcher, with a modified acceptor*)
    | N nt_val ->
      (fun accept d frag ->
        (match_nt nt_val pf match_rhs (pf nt_val)) (match_rhs pf rest_symbols accept) d frag
      )
    (*if s is terminal, try matching frag with s*)
    | T t_val ->
      (fun accept d frag ->
        match frag with
        (*empty list does not match any t_val*)
        | [] -> None
        (*if t_val matches the head of frag, keep trying the rest of symbols
          else this rhs does not match*)
        | h::suffix ->
          if h = t_val then (match_rhs pf rest_symbols) accept d suffix else None
      )

let parse_prefix gram =
  fun accept frag ->
    let ss, pf = gram in
    match_nt ss pf match_rhs (pf ss) accept [] frag