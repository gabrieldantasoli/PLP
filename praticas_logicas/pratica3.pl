incremenaPeca(P,M,G,D,'p',Desc,Qtd) :-
    P1 is P + 1,
    D9 is D + 1,
    read(A),
    incremenaPeca(P1,M,G,D9,A,Desc,Qtd).
    
incremenaPeca(P,M,G,D,'m',Desc,Qtd) :-
    M1 is M + 1,
    D8 is D + 1,
    read(A),
    incremenaPeca(P,M1,G,D8,A,Desc,Qtd).
    
incremenaPeca(P,M,G,D,'g',Desc,Qtd) :-
    G1 is G  + 1,
    D7 is D + 1,
    read(A),
    incremenaPeca(P,M,G1,D7,A,Desc,Qtd).
    
incremenaPeca(P,M,G,D,-1,Desc,Qtd) :-
    desconto(P,M,G,Total),
    Desc is Total,
    Qtd is D.
    
desconto(P,M,G,Total) :-
    descontoP(P,D1),
    D2 is D1,
    descontoM(M,D2,D3),
    D4 is D3,
    descontoG(G,D4,D5),
    Total is D5.
    
descontoP(P,8) :- P > 3, !.
descontoP(P,R) :- P < 4, R is 0.

descontoM(M,R,S) :- M > 3, S is R + 6,!.
descontoM(M,R,S) :- M < 4, S is R .

descontoG(G,R,T) :- G > 3, T is R + 4,!.
descontoG(G,R,T) :- G < 4, T is R.
    
main :- 
    read(Preco),
    read(InitialInput),
    incremenaPeca(0,0,0,0,InitialInput,Desc,Qtd),
    PrecoFinal is ((Preco*Qtd*(100-Desc))/100),
    write(PrecoFinal),
    halt.