/*1. Considere o seguinte programa:
gosta(ana, X) :-
saudavel(X) :-
    comida(X),
    \+ processada(X).
processada(pizza).
processada(hamburger).
natural(alface).
natural(morango).
comida(X) :- processada(X).
comida(X) :- natural(X).
(a) Quais as respostas do PROLOG aos objectivos:
i. ?- saudavel(X).
ii. ?- comida(X), \+ natural(X).
iii. ?- comida(X), \+ processada(X).
iv. ?- \+ processada(X), comida(X).
*/

saudavel(X) :-
    comida(X),
    \+ processada(X).
processada(pizza).
processada(hamburger).
natural(alface).
natural(morango).
comida(X) :- processada(X).
comida(X) :- natural(X).



%a) i.   X = alface; X = morango.
%   ii.  X = pizza; X = hamburger; false ---> porque \+ natural(X) pode ser provado
%   iii. X = alface; X = morango; 
%   iv. false.

/*Aritmética
(a) (MRC) Defina o predicado somaDigitos/2, tal que somaDigitos(N, S), em que
N é um inteiro positivo, significa que S é a soma dos dígitos de N. Por exemplo,
?- somaDigitos(123, S).
S=6
i. Gerando um processo recursivo.
ii. Gerando um processo iterativo.
*/

%i. recursivo

somaDigitos1(N, N) :- N < 10, !.

somaDigitos1(N, S) :-
    N >= 10,
    Dig is N mod 10,
    New is N // 10,
    somaDigitos(New, Y),
    S is Y + Dig.
    
%ii. iterativo
somaDigitos(N, S) :-
    somaDigitos(N, S, 0).

somaDigitos(0, S, S).

somaDigitos(N, S, Aux):-
    N > 0,
    Dig is N mod 10,
    New is N // 10,
    New_Aux is Aux + Dig,
    somaDigitos(New, S, New_Aux).


