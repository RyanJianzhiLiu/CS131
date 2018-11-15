let subset_test0 = subset [] []
let subset_test1 = not (subset [1;3] [])
let subset_test2 = subset [1;3;5;5;7;9] [1;3;3;5;7;9]

let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;1;1;0] [0;1]
let equal_sets_test2 = not (equal_sets [1;3;5] [1;5])

let set_union_test0 = equal_sets (set_union [0;1;3;3] []) [0;1;3]
let set_union_test1 = equal_sets (set_union [0;1;3] [2;4;5]) [0;1;2;3;4;5]

let set_intersection_test0 = equal_sets (set_intersection [1;2;3] [4;5;6]) []
let set_intersection_test1 = equal_sets (set_intersection [4;4;4] [4;5;6]) [4]

let set_diff_test0 = equal_sets (set_diff [1;2;3] [1;4;5]) [2;3]
let set_diff_test1 = equal_sets (set_diff ["131";"33";"35L"] ["33";"35L"]) ["131"]

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x *. x) 0.9 = 0.
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. x) 1.1 = infinity

let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> -x) 20 (-1) = -1
let computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> (x**10.)) 0 1. = 1.

let sqr x = x*x
let while_away_test0 =
  while_away (sqr) ((>) 1000) 2 = [2;4;16;256]
let while_away_test1 =
  while_away ((+) 10) ((>) 40) 1 = [1;11;21;31]

let rle_decode_test0 =
  rle_decode [3,"c";2,"s"] = ["c";"c";"c";"s";"s"]
let rle_decode_test1 =
  rle_decode [0,"c";0,"s"] = []

type my_nonterminals =
  | Sym1 | Sym2 | Sym3 | Sym4

let n1 = N Sym1
let n2 = N Sym2
let n3 = N Sym3
let n4 = N Sym4
let t1 = T "a"
let t2 = T "b"
let t3 = T "c"
let my_rules =
  [Sym1, [t1;t2];
   Sym1, [t1];
   Sym1, [t3;n3]; (*blind-alley*)
   Sym2, [t1;t2;n1;n4]; (*blind-alley*)
   Sym2, [t1;t2;n1;n2;n3]; (*blind-alley*)
   Sym3, [n1;n3]; (*blind-alley*)
   Sym3, [t3;n3]; (*blind-alley*)
   Sym4, [n2;n1]  (*blind-alley*)]

let my_grammar = Sym1, my_rules

let filter_blind_alleys_test0 =
  filter_blind_alleys my_grammar =
    (Sym1, [Sym1,[t1;t2]; Sym1,[t1]])
                               
                              
