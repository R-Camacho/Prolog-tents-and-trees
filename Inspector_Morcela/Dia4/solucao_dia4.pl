:- set_prolog_flag(answer_write_options,[max_depth(0)]).

/*--------------------------------------------------------------------------------
Dia 4: programaDia4(Matriz, Coordenadas, MensagemDescodificada) e verdade se
MensagemDescodificada e a mensagem que se obtem com base nos elementos que se
encontram na lista Coordenadas da lista de listas Matriz.

----------------------------------------------------------------------------------*/

programaDia4(Matriz, Coordenadas, MensagemDescodificada) :-
       programaDia4(Matriz, Coordenadas, [], MensagemDescodificada).

% Base de recursao
programaDia4(_, [], MensagemDescodificada, MensagemDescodificada) :- !.

% Corpo do programa
programaDia4(Matriz, [[Linha, Coluna]| Resto], Aux, MensagemDescodificada) :-
   nth1(Linha, Matriz, Frase),
   nth1(Coluna, Frase, Letra),
   append(Aux, [Letra], NovoAux),
   programaDia4(Matriz, Resto, NovoAux, MensagemDescodificada).


/*

Implementacao alternativa com findall:

programaDia4(Matriz, Coordenadas, MensagemDescodificada) :-
    findall(Letra,
            (member([Linha, Coluna], Coordenadas),
             nth1(Linha, Matriz, Frase),
             nth1(Coluna, Frase, Letra)),
            MensagemDescodificada).

*/
