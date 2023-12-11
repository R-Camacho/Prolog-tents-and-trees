%Rodrigo Camacho ist1110462 LEIC-A
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

/*vizinhanca/2 
vizinhanca((L, C), Vizinhanca) é verdade se Vizinhanca é uma lista ordenada de cima
para baixo e da esquerda para a direita, sem elementos repetidos, com as coordenadas das
posições imediatamente acima, imediatamente à esquerda, imediatamente à direita e imedia-
tamente abaixo da coordenada (L, C);
*/

vizinhanca((L,C), [(N,C), (L,O), (L,E), (S,C)]) :-
    N is L - 1, %norte
    O is C - 1, %oeste
    E is C + 1, %este
    S is L + 1. %sul


/*vizinhancaAlargada/2
vizinhancaAlargada((L, C), VizinhancaAlargada) é verdade se VizinhancaAlargada é
uma lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com as coordenadas anteriores e ainda as diagonais da coordenada (L, C);
*/

vizinhancaAlargada((L,C), [(N,O), (N,C), (N,E), (L,O), (L,E), (S,O), (S,C), (S,E)]) :- 
    N is L - 1, %norte
    O is C - 1, %oeste
    E is C + 1, %este
    S is L + 1. %sul

/*tamanhoTabuleiro/2
Predicado auxiliar para calcular o tamanho de um tabuleiro (supondo que será sempre quadrada)
tamanhoTabuleiro(Tab, N) é verdade se 
*/

/*
Supondo que a utilização do predicado 
*/



/*todasCelulas/2
todasCelulas(Tabuleiro, TodasCelulas) é verdade se TodasCelulas é uma lista orde-
nada de cima para baixo e da esquerda para a direita, sem elementos repetidos, com todas as
coordenadas do tabuleiro Tabuleiro;
*/

%approach usando findall/3

todasCelulas(Tabuleiro, TodasCelulas) :-
    findall((L,C), %gera lista de pares (L,C) com o nome TodasCelulas
    (nth1(L, Tabuleiro, Linha),nth1(C, Linha , _)), 
    TodasCelulas).
    %começamos a contar no 1 (nth1) porque no jogo o ponto superior esquerdo é (1, 1)


% % Base case: matriz vazia não tem coordenadas
% todasCelulas([], []).

% % Recursão - chama o for loop exterior
% todasCelulas(Tabuleiro, Coordinates) :-
%     todasCelulasExterior(Tabuleiro, 1, Coordinates). % for loop "exterior"

% % Predicado que vai iterar sobre as linhas - for loop exterior
% todasCelulasExterior([], _, []).
% todasCelulasExterior([Linha|RestoLinhas], LinhaIndex, AllCoordinates) :-
%     todasCelulasInterior(Linha, LinhaIndex, 1, LinhaCoordinates), % "chama" o for loop "interior"
%     NextLinhaIndex is LinhaIndex + 1,
%     todasCelulasExterior(RestoLinhas, NextLinhaIndex, RestoCoordinates),
%     append(LinhaCoordinates, RestoCoordinates, AllCoordinates).

% % Predicado que vai iterar sobre os elementos (colunas) numa linha - for loop interior
% todasCelulasInterior([], _, _, []).
% todasCelulasInterior([_|RestoCols], LinhaIndex, ColIndex, [(LinhaIndex, ColIndex)|Resto]) :-
%     NextColIndex is ColIndex + 1,
%     todasCelulasInterior(RestoCols, LinhaIndex, NextColIndex, RestoCoordinates).



/*todasCelulas/3
todasCelulas(Tabuleiro, TodasCelulas, Objecto) é verdade se TodasCelulas é uma
lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com todas as coordenadas do tabuleiro Tabuleiro em que existe um objecto do tipo Objecto
(neste contexto (tal como no anterior) objecto é uma tenda (t), relva (r), árvore (a) ou ainda
uma variável (por exemplo X), para indicar os espaços não preenchidos).
*/

%todasCelulas(Tabuleiro, TodasCelulas, Objecto).
    
