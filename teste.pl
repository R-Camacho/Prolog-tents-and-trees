/*
:- initialization hello_world, halt.

hello_world : -write('Hello, World!'), nl.
*/

p(X,Z) :- q(X,Y), p(Y, Z).
p(X, Y).
q(a, b).