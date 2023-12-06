% Mostra os elementos todos das listas, mesmo se estas forem muito grandes
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

/*--------------------------------------------------------------------------------
Dia 1: 

eliminaNumeros(Lista, ListaSemNumeros) e verdade se ListaSemNumeros for Lista sem numeros

Exemplo:
?- Lista = [0, 1, 4, t, 7, e, n, 6, 1, 2, h, o, 0, 8, l, i, v, 5, 6, r, 9, 1, 2, o],
eliminaNumeros(Lista, ListaSemNumeros).
...
ListaSemNumeros = [t,e,n,h,o,l,i,v,r,o].

----------------------------------------------------------------------------------*/

% Implementacao interativa, com argumento auxiliar, inicializado com a lista vazia
eliminaNumeros(Lista, ListaSemNumeros) :- eliminaNumeros(Lista, [], ListaSemNumeros).

% Base de recursao
eliminaNumeros([], ListaSemNumeros, ListaSemNumeros).

% Quando aparece um numero, ignorar
eliminaNumeros([Cabeca | Resto], Aux, ListaSemNumeros) :- 
     number(Cabeca), !,
     eliminaNumeros(Resto, Aux, ListaSemNumeros).

% Quando nao e um numero, guardar na lista Aux
eliminaNumeros([Cabeca | Resto], Aux, ListaSemNumeros) :- 
     append(Aux, [Cabeca], NovoAux),
     eliminaNumeros(Resto, NovoAux, ListaSemNumeros).

/*

Solucao alternativa, com outros predicados pre-definidos que vamos estudar em breve:

eliminaNumeros1(Lista, ListaSemNumeros) :- exclude(number, Lista, ListaSemNumeros).

*/

