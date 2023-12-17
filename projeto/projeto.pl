%Rodrigo Manuel Pita Camacho ist1110462 
%LEIC-A
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

/*todasCelulas/2
todasCelulas(Tabuleiro, TodasCelulas) é verdade se TodasCelulas é uma lista orde-
nada de cima para baixo e da esquerda para a direita, sem elementos repetidos, com todas as
coordenadas do tabuleiro Tabuleiro;
*/

%approach usando findall/3

todasCelulas(Tabuleiro, TodasCelulas) :-
    findall(    %gera lista de pares (L,C) com o nome TodasCelulas
    (L,C), 
    (nth1(L, Tabuleiro, Linha),nth1(C, Linha , _)), 
    TodasCelulas).
    %começamos a contar no 1 (nth1) porque no jogo o ponto superior esquerdo é (1, 1)


/*todasCelulas/3
todasCelulas(Tabuleiro, TodasCelulas, Objecto) é verdade se TodasCelulas é uma
lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com todas as coordenadas do tabuleiro Tabuleiro em que existe um objecto do tipo Objecto
(neste contexto (tal como no anterior) objecto é uma tenda (t), relva (r), árvore (a) ou ainda
uma variável (por exemplo X), para indicar os espaços não preenchidos).
*/

todasCelulas((Tabuleiro, TodasCelulas, Objecto)) :-
    findall(
        (L, C),
        (nth1(L, Tabuleiro, Linha),
         nth1(C, Linha, Element),
         Element == Objecto),
        TodasCelulas).

/*contaObjectos/3
Predicado auxiliar: contaObjectos(O, L, C) é verdade se existem C vezes o objeto O na lista L
*/

%Caso o Objecto seja uma variavel

contaObjectos(Objecto, L, Conta) :- 
    var(Objecto), !,  
    setof(Objecto, member(Objecto, L), L2),
    length(L2, Conta), !.


%Caso o Objecto seja uma constante
contaObjectos(Objecto, L, Conta) :-  

    exclude(var, L, Constantes), %retira as variaveis da lista
    findall(Objecto, member(Objecto, Constantes), L2),
    length(L2, Conta), !.




/*contaObjectosLinha/3
Predicado auxiliar: contaObjectosLinha(O, Tabuleiro, ContaLinhas) é verdade se a 
lista ContaLinhas é composta pelo número de vezes que o objeto O aparece em cada linha do Tabuleiro
*/
contaObjectosLinha(Tabuleiro, Objecto, ContagemLinhas):-
    maplist(contaObjectos(Objecto), Tabuleiro, ContagemLinhas).

/*contaObjectosColuna/3
Predicado auxiliar: contaObjectosColuna(O, Tabuleiro, ContaColunas) é verdade se a 
lista ContaColunas é composta pelo número de vezes que o objeto O aparece em cada coluna do Tabuleiro
*/
contaObjectosColuna(Tabuleiro, Objecto, ContagemColunas):-
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz é o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas).

/*calculaObjectosTabuleiro/4
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) é verdade se 
*/


%caso especial - o objeto e uma variavel
%calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) :-




calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) :-
    contaObjectosLinha(Tabuleiro, Objecto, ContagemLinhas), 
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz e o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas).

    
/*celulaVazia/2
celulaVazia(Tabuleiro, (L, C)) e verdade se Tabuleiro for um tabuleiro que nao tem
nada ou tem relva nas coordenadas (L, C)
*/
celulaVazia(Tabuleiro, (L, C)) :-
    nth1(L, Tabuleiro, Linha), %mais uma vez comecamos a contar no 1
    nth1(C, Linha, Celula),% Aceder a linha L e depois coluna C
    member(Celula, [r|_]).
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
    


/*
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) é ver-
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



