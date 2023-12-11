%Dia 1
/*eliminaNumeros(Lista, ListaSemNumeros)  
 
em que ListaSemNumeros é a lista resultante de eliminar de Lista os números. 
 
Exemplos: 
?- eliminaNumeros([1, o, 2, 4, l, 1, a], ListaSemNumeros). 
ListaSemNumeros = [o,l,a]. 
 
?- eliminaNumeros([1], ListaSemNumeros). 
ListaSemNumeros = [].
*/


eliminaNumeros([], []).
eliminaNumeros([H|T], Nums) :-
    number(H), !, 
    eliminaNumeros(T, Nums).

eliminaNumeros([H|T], [H|Nums]):-
    eliminaNumeros(T, Nums).


:-['dia1_testes.pl'].