%Listas

%1
ajuda(sam, frodo).
heroi(deadpool).
heroi(frodo).
heroi(X) :- ajuda(X, Y), heroi(Y).

%2 escreve lista
escreveLista([]).
escreveLista([P|R]) :- 
    writeln(P), 
    escreveLista(R).


%3 mult por N
%recursivamente
multPorN([], _, []).
multPorN([P|R], N, [X|L]) :-
    X is P * N,
    multPorN(R, N, L).

%iterativamente
multPorN2(L1, N, L2) :- multPorN2(L1, N, L2, []).
multPorN2([], _, L, L).
multPorN2([H|T], N, L2, Aux) :-
    H1 is H*N,
    append(Aux, [H1], Aux1),
    multPorN2(T, N, L2, Aux1).


% duplicaElem/2 
%recursivamente
duplicaElem([], []).
duplicaElem([P|R], [X|T]):-
    T is [P|R],
    duplicaElem(R, T).
    
