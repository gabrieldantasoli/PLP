main :- 
    read(A),
    myFloor(A,Fl),
    myCeil(A,Cl),
    myAbs(A,Ab),
    write('Floor de '),write(A),write(' = '),write(Fl),nl,
    write('Ceil de '),write(A),write(' = '),write(Cl),nl,
    write('Abs de '),write(A),write(' = '),write(Ab),nl,
    halt.
    
myAbs(X,R) :- X >= 0, R is X, !.
myAbs(X,R) :- R is X * (-1).

myFloor(X,R) :- X =:= integer(X), R is X, !.
myFloor(X,R) :- X >= 0, R is integer(X), !.
myFloor(X,R) :- R is -integer(-X+1).

myCeil(X,R) :- X =:= integer(X), R is X, !.
myCeil(X,R) :- X >= 0 , R is integer(X+1), !.
myCeil(X,R) :- R is -integer((-X)).