main :- 
    read(Matriz),
    getFirst(Matriz,F),
    getCoords(Matriz,F,1,1,1,1,R1,R2),
    write(R1),write(' '),write(R2),nl,
    halt.
    
getFirst([[F|_]|_],R) :- R is F, !.

getCoords([],_,X,Y,_,_,R1,R2) :- R1 is Y, R2 is X, !. 
getCoords([[]|T],M,X,Y,_,Py,R1,R2) :- 
    Py1 is Py + 1,
    Px1 is 1,
    getCoords(T,M,X,Y,Px1,Py1,R1,R2), !.
getCoords([[M1|T1]|T2],M,_,_,Px,Py,R1,R2) :- 
    M1 > M,
    Nm is M1,
    Px1 is Px + 1,
    X1 is Px,
    Y1 is Py,
    getCoords([T1|T2],Nm,X1,Y1,Px1,Py,R1,R2), !.
getCoords([[_|T1]|T2],M,X,Y,Px,Py,R1,R2) :- 
    Px1 is Px + 1,
    getCoords([T1|T2],M,X,Y,Px1,Py,R1,R2), !.