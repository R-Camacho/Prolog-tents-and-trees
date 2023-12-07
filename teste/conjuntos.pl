nota(ist1, 16).
nota(ist2, 18).
nota(ist3, 14).
nota_maior(A, N, V) :-
    N > V, 
    nota(A, N).

soma5([], []).
soma5([E|R], [X|S]) :- 
    X is E + 5,
    soma5(R, S). 

junta([], L, L). % uma das listas (primeira) vazia
junta([P|R], L1, [P|L2]) :- junta(R, L1, L2).

reverse([], []).
reverse([P|R], L) :- 
    reverse(R, S),
    junta(S, [P], L).

escolhe([], _, []).
escolhe([P|R], P, S ) :- escolhe(R, P, S). %se P for E 
escolhe([P|R], E, [P|S] ) :- escolhe(R, E, S). %se forem diferentes

     


