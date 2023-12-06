:- set_prolog_flag(answer_write_options,[max_depth(0)]).

/*--------------------------------------------------------------------------------
Dia 2: descodificaDia2(SeqFonte, SeqAlvo, MensagemCod, MensagemDescod)
e verdade se MensagemDescod for o resultado de aplicar o codigo que se obtem do 
alinhamento entre as listas SeqFonte e SeqAlvo aos elementos de MensagemCod

Exemplo:
?- SeqFonte = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
SeqAlvo = [o, d, e, y, ola, bye, fail, 5, a, j, 0, 45, sea, hello, hi, glory],
MensagemCod = [ola, bye, sea, 45, 0], 
descodificaDia2(SeqFonte, SeqAlvo, MensagemCod, MensagemDescod).
...
MensagemDescod = [5,6,13,12,11].
----------------------------------------------------------------------------------*/

% Implementacao interativa, com argumento auxiliar, inicializado com a lista vazia
descodificaDia2(SeqFonte, SeqAlvo, MensagemCodificada, MensagemDescodificada) :- 
      descodificaDia2(SeqFonte, SeqAlvo, MensagemCodificada, [], MensagemDescodificada). 

% Base de recursao
descodificaDia2(_, _, [], MensagemDescodificada, MensagemDescodificada) :- !.

% Caso em que o caracter existe
descodificaDia2(SeqFonte, SeqAlvo, [Cabeca | Resto], Aux, MensagemDescodificada) :-
      nth1(Index, SeqAlvo, Cabeca), !, 
      nth1(Index, SeqFonte, Codigo), !,
      append(Aux, [Codigo], NovoAux),
      descodificaDia2(SeqFonte, SeqAlvo, Resto, NovoAux, MensagemDescodificada).

% Caso o caracter nao exista
descodificaDia2(SeqFonte, SeqAlvo, [_ | Resto], Aux, MensagemDescodificada) :-
      descodificaDia2(SeqFonte, SeqAlvo, Resto, Aux, MensagemDescodificada).
      
/*

Implementacao alternativa com findall.

descodificaDia2(SeqFonte, SeqAlvo, MensagemCodificada, MensagemDescodificada) :-
    findall(ElementoFonte,
        (member(ElementoAlvo, MensagemCodificada), % para cada elemento a descodificar
         nth1(Index, SeqAlvo, ElementoAlvo),       % encontrar a sua posicao
         nth1(Index, SeqFonte, ElementoFonte)), % procurar a posicao correspondente na fonte
         MensagemDescodificada).  % Guarda na Lista MensagemDescodificada

*/