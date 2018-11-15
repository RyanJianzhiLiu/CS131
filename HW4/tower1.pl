tower(0,[],counts([],[],[],[])).
tower(N,T,counts(Top,Btm,L,R)):-
    length(T,N),
    legal_towers(N,T),
    row_counts(T,L,R),
    transpose(T,Tt),
    row_counts(Tt,Top,Btm).
    
legal_towers(_,[]).
legal_towers(N,[Row|Rest_rows]):-
    length(Row,N),
    fd_domain(Row,1,N),
    fd_all_different(Row),
    fd_labeling(Row),
    colwise_uniq(Row,Rest_rows),
    legal_towers(N,Rest_rows).

% colwise_uniq(Row,Matrix)
% each elem in Row is uniquw in its col in Matrix
colwise_uniq(_,[]).
colwise_uniq(L,[Row|Rest_rows]):-
    colwise_diff(L,Row),
    colwise_uniq(L,Rest_rows).    
colwise_diff([],[]).
colwise_diff([H1|T1],[H2|T2]):-
    H1 #\= H2,
    colwise_diff(T1,T2).

    
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
          [Rest_of_row|Rest_of_other_cols]):-
    first_col(Rest_rows,Rest_elems,Rest_of_other_cols).

row_count(Row,Count):-
    row_count(Row,0,Count).
row_count([],_,0).
row_count([H|T],Threshold,Count):-
    H #> Threshold,
    NewCount #= Count-1,
    row_count(T,H,NewCount).
row_count([H|T],Threshold,Count):-
    H #< Threshold,
    row_count(T,Threshold,Count).

row_counts([],[],[]).
row_counts([Row|Rest_rows],[HL|TL],[HR|TR]):-
    row_count(Row,HL),
    reverse(Row,Row_r),
    row_count(Row_r,HR),
    row_counts(Rest_rows,TL,TR).
