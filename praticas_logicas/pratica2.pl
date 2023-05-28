main :- 
    read(List),
    read(N),
    operations(List,0,N,R),
    write(R),
    halt.
    

operations([],S,A,R) :- R is A + S - S, !.
operations([H|T],S,A,R) :- 
    H =:= 0,
    S1 is S - S,
    operations(T,S1,A,R), !.
operations([H|T],S,A,R) :- 
    S =:= 0, 
    A1 is A + H, 
    S1 is S +1,
    operations(T,S1,A1,R), !.
operations([H|T],S,A,R) :- 
    S =:= 1, 
    A1 is A - H, 
    S1 is S +1,
    operations(T,S1,A1,R), !.
operations([H|T],S,A,R) :- 
    S =:= 2, 
    A1 is A * H, 
    S1 is S +1,
    operations(T,S1,A1,R), !.
operations([H|T],S,A,R) :- 
    S =:= 3, 
    A1 is A / H, 
    S1 is 0,
    operations(T,S1,A1,R), !.