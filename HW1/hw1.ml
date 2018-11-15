let rec subset a b =
  match a with
  | [] -> true
  | first_a::rest_a ->
     if (List.mem first_a b) then (subset rest_a b) else false

    
let equal_sets a b =
  (subset a b) && (subset b a)

  
let rec uniq ls =
  match ls with
  | [] -> []
  | first::rest ->
     if (List.mem first rest) then uniq rest else first::(uniq rest)

let set_union a b = uniq (a@b)

                  
let rec common a b =                   
  match a with
  | [] -> []
  | first_a::rest_a ->
     if (List.mem first_a b) then first_a::(common rest_a b) else (common rest_a b)

let set_intersection a b = common (uniq a) (uniq b)

                         
let rec uniq_first a b =
  match a with
  | [] -> []
  | first_a::rest_a ->
     if (List.mem first_a b) then (uniq_first rest_a b) else first_a::(uniq_first rest_a b)

let set_diff a b = uniq_first (uniq a) (uniq b)


let rec computed_fixed_point eq f x =
  let fx = f x in
  if (eq x fx) then x else computed_fixed_point eq f fx


let rec fun_exp f p x =
  if (p = 0) then x else f (fun_exp f (p-1) x)
  
let rec computed_periodic_point eq f p x =
  let f_p_x = fun_exp f p x in
  if (eq x f_p_x) then x else computed_periodic_point eq f p (f x)


let rec while_away s p x =
  if (p x) then x::(while_away s p (s x)) else []


let rec rle_decode lp =
  match lp with
  | [] -> []
  | first::rest ->
     let (cnt, elem) = first in
     if (cnt > 0) then elem::(rle_decode (((cnt-1), elem)::rest)) else (rle_decode rest)


type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*
fun update:
    find rules:(lhs,rhs) such that rhs consists of only symbols that can terminate, mark lhs as "can terminate"
repeat until reaching a "fixed point" 
 *)

let rec find_t_symbols symbol_ls =
  match symbol_ls with
     | [] -> []
     | [_] ->
        (match (List.hd symbol_ls) with
         | T t_symbol -> symbol_ls
         | N n_symbol -> [])
     | first_symbol::rest_symbols ->
        (find_t_symbols [first_symbol]) @ (find_t_symbols rest_symbols)
    
let rec initialize_terminables rules =
  match rules with
  | [] -> []
  | first_rule::rest_rules ->
     let (lhs, rhs) = first_rule in
     uniq ((find_t_symbols rhs) @ (initialize_terminables rest_rules))

let rec is_terminable rhs terminables =
  match rhs with
  | [] -> true
  | first_symbol::rest ->
     if (List.mem first_symbol terminables) then (is_terminable rest terminables) else false

let rec update_terminables rules terminables =
  match rules with
  | [] -> terminables
  | first_rule::rest ->
     let (lhs, rhs) = first_rule in
     if (is_terminable rhs terminables)
     then uniq ((N lhs)::(update_terminables rest terminables))
     else (update_terminables rest terminables)

let rec terminable_rules rules terminables =
  match rules with
  | [] -> []
  | first_rule::rest ->
     let (lhs, rhs) = first_rule in
     if (is_terminable rhs terminables)
     then first_rule::(terminable_rules rest terminables)
     else (terminable_rules rest terminables)
     
let filter_blind_alleys g =
  let start_symbol, rules = g in
  let terminables = computed_fixed_point equal_sets (update_terminables rules) (initialize_terminables rules) in
  (start_symbol, (terminable_rules rules terminables))

  
;;
