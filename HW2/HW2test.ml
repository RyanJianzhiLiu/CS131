let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type english_nonterminal =
	| Sentence | Noun_Phrase | Verb_Phrase | Noun | Verb | Adj | Adv

let simple_English_grammar =
	(Sentence,
	 function
		| Sentence ->
			[[N Adj];
			 [N Noun_Phrase];
			 [N Noun_Phrase; N Verb_Phrase];
			 [N Noun_Phrase; N Verb_Phrase; N Noun_Phrase]]
		| Noun_Phrase ->
			[[N Noun];
			 [N Adj; N Noun_Phrase];
			 [N Adv; N Adj; N Noun_Phrase]]
		| Verb_Phrase ->
			[[N Verb];
			 [N Verb; N Adv];
			 [N Adv; N Verb]]
		| Noun ->
			[[T"apple"];[T"water"];[T"astronaut"]]
		| Verb ->
			[[T"flies"];[T"sleeps"];[T"enjoys"]]
		| Adj ->
			[[T"multi-dimensional"];[T"liquid"];[T"indifferent"]]
		| Adv ->
			[[T"incredibly"];[T"happily"]])

let frag1 = ["incredibly";"multi-dimensional";"liquid";"apple";"flies";"happily"]
let frag2 = ["indifferent";"astronaut";"enjoys";"happily";"indifferent";"water"]

let test_1 = 
	((parse_prefix simple_English_grammar accept_all frag1)
	= Some
	([(Sentence,[N Noun_Phrase]);
 	(Noun_Phrase,[N Adv; N Adj; N Noun_Phrase]);
 	(Adv, [T"incredibly"]);
 	(Adj, [T"multi-dimensional"]);
 	(Noun_Phrase, [N Adj; N Noun_Phrase]);
 	(Adj, [T"liquid"]);
 	(Noun_Phrase, [N Noun]);
 	(Noun, [T"apple"])],
	["flies";"happily"]))

let test_2 = 
	((parse_prefix simple_English_grammar accept_empty_suffix frag2)
	= Some
	([(Sentence,[N Noun_Phrase; N Verb_Phrase; N Noun_Phrase]);
 	(Noun_Phrase,[N Adj; N Noun_Phrase]);
 	(Adj, [T"indifferent"]);
 	(Noun_Phrase, [N Noun]);
 	(Noun, [T"astronaut"]);
 	(Verb_Phrase, [N Verb]);
 	(Verb, [T"enjoys"]);
 	(Noun_Phrase, [N Adv; N Adj; N Noun_Phrase]);
 	(Adv, [T"happily"]);
 	(Adj, [T"indifferent"]);
 	(Noun_Phrase, [N Noun]);
 	(Noun, [T"water"])],
	[]))