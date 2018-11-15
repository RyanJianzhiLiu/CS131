tower(N,T,counts(Top,Btm,L,R)) :-
    square(T,N),
    square(Tt,N),
    legal_rows(N,Tt),
    legal_rows(N,T),
    build(T),
    build(Tt),
    horizontal_counts(N,T,L,R),
    transpose(T,Tt),
    horizontal_counts(N,Tt,Top,Btm),
    legal_counts(N,counts(Top,Btm,L,R)).

square(T,N) :-
    length(T,N),
    uni_len(T,N).
uni_len([],_).
uni_len([This_row|Rest],N):-    
    length(This_row,N),
    uni_len(Rest,N).

different([],[]).
different([Head1|Rest1],[Head2|Rest2]):-
    Head1 #\= Head2,
    different(Rest1,Rest2).
    
unique([],_).
unique(_,[]).  
unique(L,[Head|Rest]):-
    different(L,Head),
    unique(L,Rest).

legal_T(_,[]).
legal_T(N,[This_row|Rest]) :-
    fd_domain(This_row,1,N),
    fd_all_different(This_row),
    fd_labeling(This_row),
    unique(This_row, Rest),
    legal_T(N,Rest).

build([]).
build([H|T]) :-
    fd_labeling(H),
    build(T).
%transpose(A,At)
transpose([This_row|Rest_rows], At) :-
    transpose(This_row, [This_row|Rest_rows], At).

transpose([],_,[]).
transpose([_|Rest_of_row], Rest_cols, [At|Ats]) :-
    first_col(Rest_cols, At, NewRest_cols),
    transpose(Rest_of_row, NewRest_cols, Ats).    
    
% first_col(Matrix, First_col, Other_cols)
first_col([],[],[]).
first_col([[Elem|Rest_of_row]|Rest_rows],
	  [Elem|Rest_elems],
	  [Rest_of_row|Rest_of_other_cols]) :-
    first_col(Rest_rows,Rest_elems,Rest_of_other_cols).


legal_counts(N,counts(Top,Btm,L,R)) :-
    legal_count(N,Top),
    legal_count(N,Btm),
    legal_count(N,L),
    legal_count(N,R).

legal_count(N,Count) :-
    length(Count,N),
    fd_domain(Count,1,N).

horizontal_counts(_,[],[],[]).
horizontal_counts(N,[This_row|Rest_rows],
		  [This_L_count|Rest_of_L],
		  [This_R_count|Rest_of_R]) :-
    visible_count(N,This_row,This_L_count),!,
    reverse(This_row,Reversed_row),
    visible_count(N,Reversed_row,This_R_count),!,
    horizontal_counts(N,Rest_rows,Rest_of_L,Rest_of_R).
    
% count visible towers in a row from left to right
% visible_count(N, Row, Count)
visible_count(N, Row, Count) :-
    visible_all(N,Row,Subseq),
    length(Subseq,Count).
    
visible_all(N,[Head_of_row|Rest_of_row],
	    [Head_of_subseq|Rest_of_subseq]) :-
    visible(Rest_of_row,Rest_of_subseq,Head_of_subseq),
    Head_of_row #= Head_of_subseq,
    member(N,[Head_of_subseq|Rest_of_subseq]).
    
visible(_,[],_).
visible(Row,[Visible|Rest_Vs],Threshold) :-
    first_larger_than(Row,Visible,Threshold),
    visible(Row,Rest_Vs,Visible).
    
first_larger_than([Head|_],L,Val) :-
    Head #> Val,
    L = Head.
first_larger_than([Head|Rest],L,Val) :-
    Head #=< Val,
    first_larger_than(Rest,L,Val).
