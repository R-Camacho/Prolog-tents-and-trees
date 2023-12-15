%Corte
/*1. (MRC) Considere o seguinte programa:
p(X,Y) :- q(X),r(Y).
p(5,z).
q(1).
q(2).
r(a).
r(b).
(a) Indique todas as respostas do Prolog ao objetivo p(X,Y).
(b) Suponha que a primeira cláusula é substituída por
p(X,Y) :- !,q(X), r(Y).
Indique todas as respostas do Prolog ao objetivo p(X,Y).
(c) Suponha que a primeira cláusula é substituída por
p(X,Y) :- q(X),!,r(Y).
Indique todas as respostas do Prolog ao objetivo p(X,Y).
(d) Suponha que a primeira cláusula é substituída por
p(X,Y) :- q(X),r(Y),!.
Indique todas as respostas do Prolog ao objetivo p(X,Y).
*/
p(X,Y) :- q(X),r(Y), !.
p(5,z).
q(1).
q(2).
r(a).
r(b).


%a) X=1, Y=a; X=1, Y=b; X=2, Y=a; X=2, Y=b; X=5, Y=z.

%b) X=1, Y=a; X=1, Y=b; X=2, Y=a; X=2, Y=b;

%c) X=1, Y=a; X=1, Y=b.

%d) X=1, Y=a; X=1, Y=b; X=2, Y=a; X=2, Y=b;


/*
2. (MRC) Considere o seguinte programa:
p(2, 3).
p(X,Y) :- q(X), r(Y).
q(1).
q(2).
q(3).
r(1).
r(3).
(a) Indique todas as respostas do Prolog ao objetivo p(X,Y).
(b) Introduza um operador de corte para obter apenas a primeira resposta.
(c) Introduza um operador de corte para obter apenas as 3 primeiras respostas.
(d) Introduza um operador de corte para obter apenas as 2 primeiras respostas.
(e) Introduza um operador de corte para obter apenas as 5 primeiras respostas.
*/

% p(2, 3).
% p(X,Y) :- q(X), r(Y).
% q(1).
% q(2).
% q(3).
% r(1).
% r(3).

%a) 
/*
X=2, Y=3; 
X=1, Y=1; 
X=1, Y=3; 
X=2, Y=1; 
X=3, Y=1; 
X=3, Y=3;
*/


%b) p(2, 3) :- !.

%c) p(X,Y) :- q(X), !,  r(Y).

%d) p(X,Y) :- q(X), r(Y), !.

%e) q(2) :- !.


%Listas
/*1. (MRC) Deparas-te com o predicado insereOrd/3, tal que insereOrd(E, L1, L2)
(em que E é um inteiro e L1 e L2 são listas de inteiros ordenadas de forma ascendente), significa que L2 é o resultado de inserir E em L1. Por exemplo,
?- insere_ordenado(5, [1,2,3,6,9], L).
L = [1, 2, 3, 5, 6, 9] .
?- insere_ordenado(0, [1,2,3,6,9], L).
L = [0, 1, 2, 3, 6, 9] .
?- insere_ordenado(10, [1,2,3,6,9], L).
L = [1, 2, 3, 6, 9, 10] .
?- insere_ordenado(3, [1,2,3,6,9], L).
L = [1, 2, 3, 3, 6, 9] .
(a) (MRC) Implementa o predicado insereOrd/3 gerando um processo recursivo
(o iterativo é menos giro, mas também o podes fazer)
(b) Implementa o predicado insereOrd/3 usando funcionais (findall, maplist, include, exclude, ...).
*/

% a) recursivamente
%base case
insereOrd(E, [], [E]).

%se E for menor ou igual que o primeiro elemento
insereOrd(E, [P|R], [E|Tail]) :-
    E =< P,
    insereOrd(P, R, Tail).

insereOrd(E, [P|R], [P|Tail]) :-
    E > P,
    insereOrd(E, R, Tail).

%b) com funcionais

insere_ordenado(E, L1, L2) :-
    findall(X, (member(X, L1), X =< E), Menores), 
    findall(X, (member(X, L1), X > E), Maiores), 
    append(Menores, [E|Maiores], L2). %junta


/*2. (MRC) Gerando um processo iterativo, implementa o predicado comp_maior_lista/2,
tal que comp_maior_lista(L, C), em que L é uma lista de listas, significa que o
maior comprimento das listas de L é C. Por exemplo,
?- compMaiorLista([[1,2,3], [4,3,1,3], []], C).
C=4
*/

comp_maior_lista([H|T], C):-
    length([H|T], Comp),
    comp_maior_lista([H|T], Comp, C).


comp_maior_lista([], C, C).

comp_maior_lista([H|R], Comp_act, C ):-
    length(H, Comp_H), 
    Comp_H > Comp_act,
    comp_maior_lista(R, Comp_H ,C).

comp_maior_lista([H|R], Comp_act, C) :-
    length(H, Comp_H), 
    Comp_H =< Comp_act,
    comp_maior_lista(R, Comp_act, C).

