%Rodrigo Manuel Pita Camacho ist1110462 
%LEIC-A
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

/*vizinhanca/2 
vizinhanca((L, C), Vizinhanca) e verdade se Vizinhanca e uma lista ordenada de cima
para baixo e da esquerda para a direita, sem elementos repetidos, com as coordenadas das
posicoes imediatamente acima, imediatamente a esquerda, imediatamente a direita e 
imediatamente abaixo da coordenada (L, C);
*/

vizinhanca((L,C), [(N,C), (L,O), (L,E), (S,C)]) :-
    N is L - 1, %norte
    O is C - 1, %oeste
    E is C + 1, %este
    S is L + 1. %sul


/*vizinhancaAlargada/2
vizinhancaAlargada((L, C), VizinhancaAlargada) e verdade se VizinhancaAlargada e
uma lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com as coordenadas anteriores e ainda as diagonais da coordenada (L, C);
*/
vizinhancaAlargada((L,C), [(N,O), (N,C), (N,E), (L,O), (L,E), (S,O), (S,C), (S,E)]) :- 
    N is L - 1, %norte
    O is C - 1, %oeste
    E is C + 1, %este
    S is L + 1. %sul

/*todasCelulas/2
todasCelulas(Tabuleiro, TodasCelulas) e verdade se TodasCelulas e uma lista ordenada
de cima para baixo e da esquerda para a direita, sem elementos repetidos, com todas as
coordenadas do tabuleiro Tabuleiro;
*/

%approach iterativo usando findall/3

todasCelulas(Tabuleiro, TodasCelulas) :- 
    todasCelulasAux(Tabuleiro, TodasCelulas, 1), !.

todasCelulasAux(Tabuleiro, [], Prox_linha):-
    length(Tabuleiro, N), % N - numero de linhas
    Prox_linha is N + 1, !.

todasCelulasAux(Tabuleiro, TodasCelulas, Num_linha) :-
    nth1(Num_linha, Tabuleiro, Linha_atual),
    findall((Num_linha, C), nth1(C, Linha_atual, _), Coords_linha),
    Prox_linha is Num_linha + 1,
    todasCelulasAux(Tabuleiro, New, Prox_linha),
    append(Coords_linha, New, TodasCelulas).


/*todasCelulas/3
todasCelulas(Tabuleiro, TodasCelulas, Objecto) e verdade se TodasCelulas e uma
lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com todas as coordenadas do tabuleiro Tabuleiro em que existe um objecto do tipo Objecto
constantes t, r, a ou uma variavel (para indicar os espacos nao preenchidos).
*/

todasCelulas(Tabuleiro, TodasCelulas, Objecto) :-
    (var(Objecto) -> 
    %caso especial: o ultimo argumento dado (Objeto) e um variavel
    %queremos as celulas vazias
        todasCelulasVazias(Tabuleiro, TodasCelulas, 1), !
        ;
        todasCelulasAux(Tabuleiro, TodasCelulas, Objecto, 1), !
    ).

%semelhante ao predicado anterior, com approach iterativo usando bagof/3 findall/3 

todasCelulasAux(Tabuleiro, [], _, Prox_linha):- 
    length(Tabuleiro, N),  % N - numero de linhas
    Prox_linha is N + 1, !.   
        
todasCelulasAux(Tabuleiro, TodasCelulas, Objecto, Num_linha) :-
    nth1(Num_linha, Tabuleiro, Linha_atual), 
    findall((Num_linha, C), (nth1(C, Linha_atual, Celula), Celula == Objecto), Coords_linha),
    Prox_linha is Num_linha + 1,
    todasCelulasAux(Tabuleiro, New, Objecto, Prox_linha),
    append(Coords_linha, New, TodasCelulas).


todasCelulasVazias(Tabuleiro, [], Prox_linha) :-
    length(Tabuleiro, N),
    Prox_linha is N + 1, !.

todasCelulasVazias(Tabuleiro, TodasCelulas, Num_linha) :-
    nth1(Num_linha, Tabuleiro, Linha_atual), 
    findall((Num_linha, C), (nth1(C, Linha_atual, Objecto), var(Objecto)), Coords_linha),
    Prox_linha is Num_linha + 1,
    todasCelulasVazias(Tabuleiro, New, Prox_linha),
    append(Coords_linha, New, TodasCelulas).


/*contaObjectos/3
Predicado auxiliar: contaObjectos(O, L, C) e verdade se existem C vezes o objeto O na lista L
*/

%Caso o Objecto seja uma variavel
contaObjectos(Objecto, L, Conta) :- 
    var(Objecto), !,
    exclude(nonvar, L, Variaveis), %retira as constantes da lista  
    length(Variaveis, Conta), !.

%Caso o Objecto seja uma constante
contaObjectos(Objecto, L, Conta) :-  
    exclude(var, L, Constantes), %retira as variaveis da lista
    findall(Objecto, member(Objecto, Constantes), L2),
    length(L2, Conta), !.


/*contaObjectosLinha/3
Predicado auxiliar: contaObjectosLinha(O, Tabuleiro, ContaLinhas) e verdade se a 
lista ContaLinhas e composta pelo numero de vezes que o objeto O aparece em cada linha do Tabuleiro
*/

contaObjectosLinha(Tabuleiro, Objecto, ContagemLinhas):-
    maplist(contaObjectos(Objecto), Tabuleiro, ContagemLinhas).

/*contaObjectosColuna/3
Predicado auxiliar: contaObjectosColuna(O, Tabuleiro, ContaColunas) e verdade se a 
lista ContaColunas e composta pelo numero de vezes que o objeto O aparece em cada coluna do Tabuleiro
*/
contaObjectosColuna(Tabuleiro, Objecto, ContagemColunas):-
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz e o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas).


/*calculaObjectosTabuleiro/4
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) e verdade se 
*/

calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) :-
    contaObjectosLinha(Tabuleiro, Objecto, ContagemLinhas), 
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz e o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas).

    
/*celulaVazia/2
celulaVazia(Tabuleiro, (L, C)) e verdade se Tabuleiro for um tabuleiro que nao tem
nada ou tem relva nas coordenadas (L, C)
*/
celulaVazia(Tabuleiro, (L, C)) :-  
    %caso as coordenadas sejam fora do tabuleiro
    length(Tabuleiro, N),
    ((L < 1; L > N ; C < 1; C > N), ! ;
    %----------------------------------
    nth1(L, Tabuleiro, Linha), %mais uma vez comecamos a contar no 1
    nth1(C, Linha, Celula),% Aceder a linha L e depois coluna C
    (var(Celula), !; Celula == r)).
    %ve se Celula e relva (r) ou esta vazia (neste caso uma variavel)
    
    
/*insereObjectoCelula/3
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) e verdade se Tabuleiro e um
tabuleiro e (L, C) sao as coordenadas onde queremos inserir o objecto TendaOuRelva
*/


insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)):-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Celula),
    ((nonvar(Celula)); nth1(C, Linha, TendaOuRelva)), !. 
    %se a celula estiver ocupada (for uma constante), T unifica consigo proprio 
    %caso contrario e colocada a TendaOuRelva no Tabuleiro
    
/*insereObjectoEntrePosicoes/4
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) e ver-
dade se Tabuleiro e um tabuleiro, e (L, C1) e (L, C2) sao as coordenadas, na Linha L,
entre as quais (incluindo) se insere o objecto TendaOuRelva.
*/
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), C1), !.

insereObjectoEntrePosicoes(_, _, (L, _), (L, C2), Prox_C2):-
    Prox_C2 is C2 + 1. %Para quando a ultima coluna for ultrapassada

insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Contador):-
    Atual_C is Contador,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, Atual_C)),
    Prox_C is Contador + 1,
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Prox_C).



/*relva/1
relva(Puzzle) é verdade se Puzzle e um puzzle que, após a aplicação do predicado, tem
relva em todas as linhas/colunas cujo número de tendas já atingiu o número de tendas possível
nessas linhas/colunas;
*/

relva(Puzzle) :-
    relva(Puzzle, 1).

relva(Puzzle, Linha_atual):-
    puzzle(_,Tabuleiro, TendaLinhas, TendaColunas).
