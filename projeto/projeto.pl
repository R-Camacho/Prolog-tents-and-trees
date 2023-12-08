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

/*todasCelulas/2
todasCelulas(Tabuleiro, TodasCelulas) é verdade se TodasCelulas é uma lista orde-
nada de cima para baixo e da esquerda para a direita, sem elementos repetidos, com todas as
coordenadas do tabuleiro Tabuleiro;
*/

% Base case: matriz vazia não tem coordenadas
todasCelulas([], []).

% Recursão - chama o for loop exterior
todasCelulas(Tabuleiro, Coordinates) :-
    todasCelulasExterior(Tabuleiro, 1, Coordinates). % for loop "exterior"

% Predicado que vai iterar sobre as linhas 
todasCelulasExterior([], _, []).
todasCelulasExterior([Row|RestRows], RowIndex, AllCoordinates) :-
    todasCelulasInterior(Row, RowIndex, 1, RowCoordinates), % for loop "interior"
    NextRowIndex is RowIndex + 1,
    todasCelulasExterior(RestRows, NextRowIndex, RestCoordinates),
    append(RowCoordinates, RestCoordinates, AllCoordinates).

% Predicado que vai iterar sobre os elementos (colunas) numa linha 
todasCelulasInterior([], _, _, []).
todasCelulasInterior([_|RestCols], RowIndex, ColIndex, [(RowIndex, ColIndex)|RestCoordinates]) :-
    NextColIndex is ColIndex + 1,
    todasCelulasInterior(RestCols, RowIndex, NextColIndex, RestCoordinates).

