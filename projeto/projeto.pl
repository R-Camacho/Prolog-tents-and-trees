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
    S is L + 1, !. %sul


/*vizinhancaAlargada/2
vizinhancaAlargada((L, C), VizinhancaAlargada) e verdade se VizinhancaAlargada e
uma lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com as coordenadas anteriores e ainda as diagonais da coordenada (L, C);
*/
vizinhancaAlargada((L,C), [(N,O), (N,C), (N,E), (L,O), (L,E), (S,O), (S,C), (S,E)]) :- 
    N is L - 1, %norte
    O is C - 1, %oeste
    E is C + 1, %este
    S is L + 1, !. %sul

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
    append(Coords_linha, New, TodasCelulas), !.


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

%semelhante ao predicado anterior, com approach iterativo usando findall/3 

todasCelulasAux(Tabuleiro, [], _, Prox_linha):- 
    length(Tabuleiro, N),  % N - numero de linhas
    Prox_linha is N + 1, !.   
        
todasCelulasAux(Tabuleiro, TodasCelulas, Objecto, Num_linha) :-
    nth1(Num_linha, Tabuleiro, Linha_atual), 
    findall((Num_linha, C), (nth1(C, Linha_atual, Celula), Celula == Objecto), Coords_linha),
    Prox_linha is Num_linha + 1,
    todasCelulasAux(Tabuleiro, New, Objecto, Prox_linha),
    append(Coords_linha, New, TodasCelulas), !.


todasCelulasVazias(Tabuleiro, [], Prox_linha) :-
    length(Tabuleiro, N),
    Prox_linha is N + 1, !.

todasCelulasVazias(Tabuleiro, TodasCelulas, Num_linha) :-
    nth1(Num_linha, Tabuleiro, Linha_atual), 
    findall((Num_linha, C), (nth1(C, Linha_atual, Objecto), var(Objecto)), Coords_linha),
    Prox_linha is Num_linha + 1,
    todasCelulasVazias(Tabuleiro, New, Prox_linha),
    append(Coords_linha, New, TodasCelulas), !.


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
    maplist(contaObjectos(Objecto), Tabuleiro, ContagemLinhas), !.

/*contaObjectosColuna/3
Predicado auxiliar: contaObjectosColuna(O, Tabuleiro, ContaColunas) e verdade se a 
lista ContaColunas e composta pelo numero de vezes que o objeto O aparece em cada coluna do Tabuleiro
*/
contaObjectosColuna(Tabuleiro, Objecto, ContagemColunas):-
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz e o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas), !.


/*calculaObjectosTabuleiro/4
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) e verdade se Tabuleiro for um tabuleiro, 
Objecto for o tipo de objecto que se procura, e ContagemLinhas e ContagemColunas forem, 
respectivamente, listas com o número desses objectos por linha e por coluna.
*/

calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) :-
    contaObjectosLinha(Tabuleiro, Objecto, ContagemLinhas), 
    transpose(Tabuleiro, Transposto), % contar as colunas de uma matriz e o mesmo que contar as linhas da sua transposta
    contaObjectosLinha(Transposto, Objecto, ContagemColunas), !.

    
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
    (var(Celula), !; Celula == r)), !.
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
    insereObjectoEntrePosicoesAux(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), C1), !.

insereObjectoEntrePosicoesAux(_, _, (L, _), (L, C2), Prox_C2):-
    Prox_C2 is C2 + 1, !. %Para quando a ultima coluna for ultrapassada

insereObjectoEntrePosicoesAux(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Contador):-
    Atual_C is Contador,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, Atual_C)),
    Prox_C is Contador + 1,
    insereObjectoEntrePosicoesAux(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Prox_C), !.

/*relva/1
relva(Puzzle) é verdade se Puzzle e um puzzle que, após a aplicação do predicado, tem
relva em todas as linhas/colunas cujo número de tendas já atingiu o número de tendas possível
nessas linhas/colunas;
*/

relva(Puzzle) :-
    Puzzle = (Tabuleiro, Lista_tendas_linhas, Lista_tendas_colunas),
    length(Tabuleiro, N),
    contaObjectosLinha(Tabuleiro, t, Tendas_Colocadas),
    relva(Tabuleiro, Tendas_Colocadas, Lista_tendas_linhas, N, 1),
    transpose(Tabuleiro, Transposto),
    contaObjectosLinha(Transposto, t, Tendas_ColocadasColunas),
    relva(Transposto, Tendas_ColocadasColunas, Lista_tendas_colunas, N, 1), !.

%caso terminal da iteracao
relva(Tabuleiro, _, _, _, N_Linha) :-
    length(Tabuleiro, N), % N - numero de linhas
    N_Linha is N + 1, !.

relva(Tabuleiro, Tendas_Colocadas, Lista_tendas, N, N_Linha):-
    (%se a linha estiver completa
    
    nth1(N_Linha, Tendas_Colocadas, Tendas_nesta_linha), %descobrir o numero de tendas existentes por linha

    nth1(N_Linha, Lista_tendas, N_Tendas_a_colocar), %comparar com o numero de tendas necessario por linha
    Tendas_nesta_linha == N_Tendas_a_colocar,

    %se estiver completa, coloca relva nas celulas vazias
    insereObjectoEntrePosicoes(Tabuleiro, r, (N_Linha, 1), (N_Linha, N)),

    %avanca para a prox iteracao
    Prox_linha is N_Linha + 1,
    relva(Tabuleiro, Tendas_Colocadas, Lista_tendas, N, Prox_linha), !
    )

    ; %caso contrario apenas avanca para a prox iteracao
    Prox_linha is N_Linha + 1,
    relva(Tabuleiro, Tendas_Colocadas, Lista_tendas, N, Prox_linha), !.

/*inacessiveis/1
inacessiveis(Tabuleiro) e verdade se Tabuleiro e um tabuleiro que, apos a aplicacao do
predicado, tem relva em todas as posições inacessiveis
*/

%posicao inacessivel e uma celula que nao esta na vizinanca de nenhuma arvore

inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro, Vazias, _),
    inacessiveis(Tabuleiro, Vazias), !.

inacessiveis(_, []):- !.

inacessiveis(Tabuleiro, [Celula | R]) :-


    (%se a celula for inacessivel:
    vizinhanca(Celula, Vizinhanca),
    maplist(celulaVazia(Tabuleiro), Vizinhanca), %so passa se todas forem vazias
    insereObjectoCelula(Tabuleiro, r, Celula), 
    
    %avanca para a prox iteracao
    inacessiveis(Tabuleiro, R), !
    )
    ; %caso contrario apenas avanca para a prox iteracao
    inacessiveis(Tabuleiro, R), !.



/*aproveita/1
aproveita(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado,
tem tendas em todas as linhas e colunas as quais faltavam colocar X tendas e que tinham 
exactamente X posicoes livres. Este predicado deve ser implementado resolvendo as
linhas, fazendo novas contagens, e resolvendo as colunas; nao recursivo;
*/

aproveita(Puzzle) :-
    Puzzle = (Tabuleiro, Lista_tendas_linhas, Lista_tendas_colunas),
    length(Tabuleiro, N),
    contaObjectosLinha(Tabuleiro, t, Tendas_Colocadas_Linha),
    aproveitaLinhas(Tabuleiro, Lista_tendas_linhas, Tendas_Colocadas_Linha, N,  1),
    transpose(Tabuleiro, Transposto),
    contaObjectosLinha(Transposto, t, Tendas_Colocadas_Coluna),
    aproveitaLinhas(Transposto, Lista_tendas_colunas, Tendas_Colocadas_Coluna, N,  1), !.

%caso terminal da iteracao
aproveitaLinhas(Tabuleiro, _, _, _, N_Linha) :-
    length(Tabuleiro, N), % N - numero de linhas
    N_Linha is N + 1, !.


aproveitaLinhas(Tabuleiro, Lista_Tendas_linhas, Tendas_Colocadas, N, N_Linha) :-
    
    (%se o numero de posicoes livres na linha e igual ao numero de tendas a colocar
    nth1(N_Linha, Tendas_Colocadas, N_Tendas_Colocadas), %descobrir o numero de tendas existentes por linha
    
    
    contaObjectosLinha(Tabuleiro, _, Livres), 
    nth1(N_Linha, Livres, N_Livres),  %descobrir o numero de posicoes livres por linha

    nth1(N_Linha, Lista_Tendas_linhas, N_Tendas_a_colocar), 
    
    Tendas_a_Faltar is N_Tendas_a_colocar - N_Tendas_Colocadas,
    Tendas_a_Faltar == N_Livres, %comparar com o numero de tendas por colocar 

    %se for linha "aproveitavel", coloca tendas nas celulas vazias
    
    insereObjectoEntrePosicoes(Tabuleiro, t, (N_Linha, 1), (N_Linha, N)),

    %avanca para a prox iteracao
    Prox_linha is N_Linha + 1,
    aproveitaLinhas(Tabuleiro, Lista_Tendas_linhas, Tendas_Colocadas, N, Prox_linha), !

    ) 
    ; %caso contrario apenas avanca para a prox iteracao
    Prox_linha is N_Linha + 1,
    aproveitaLinhas(Tabuleiro, Lista_Tendas_linhas, Tendas_Colocadas, N, Prox_linha), !.



/*limpaVizinhancas/1
limpaVizinhancas(Puzzle) e verdade se Puzzle e um puzzle que, após a aplicação do
predicado, tem relva em todas as posições a volta de uma tenda (vizinhanca alargada)
*/

% limpaVizinhancas(Puzzle):-
%     Puzzle = (Tabuleiro, _, _),
%     length(Tabuleiro, N),
%     todasCelulas(Tabuleiro, Tendas, t),
%     write(Tendas), nl,
%     limpaVizinhancas(Tabuleiro, N, Tendas),
%     write('aa').

% limpaVizinhancas(_, []) :- !.

% limpaVizinhancas(Tabuleiro, N, [Tenda|R]):-
%     write(Tenda), nl,
%     length(Tabuleiro, N),
%     vizinhancaAlargada(Tenda, Viz),
%     ocupaVizinhanca(Tenda, Viz, Tabuleiro, N),
%     limpaVizinhancas(Tabuleiro, R), !.


% %predicado auxiliar que nao falha quando select(Elem, Lista, R) e chamado e Elem nao esta na Lista 
% select_true(Elem, Lista, R):-
%     select(Elem, Lista, R), !.
% select_true(_, Lista, Lista).

% ocupaVizinhanca((L,C), Viz, Tabuleiro, Tam):-





    % Viz = [(N,O), (N,C), (N,E), (L,O), (L,E), (S,O), (S,C), (S,E)],
    % write(Viz), nl,
    % write([(N,O), (N,C), (N,E), (L,O), (L,E), (S,O), (S,C), (S,E)]), 
    % write(1), nl,
    
    % (
    % %se estiver na primeira linha
    % (write(a), nl, 
    % L == 1, 
    % write(primlin), nl,
    % write(N),nl,
    % %Remove os tres elementos superiores a tenda da lista da vizinhanca
    % write(Viz),nl,
    % select_true((N,O), Viz, Temp1Cima),
    % select_true((N,C), Temp1Cima, Temp2Cima),
    % select_true((N,E), Temp2Cima, Temp3Cima),
    % write(Temp3Cima), nl,
    % write(tempemcima), nl

    % );
    % (write(b), nl,%se estiver na primeira coluna 
    % C == 1,
    % write(primcol), nl,
    
    % %Remove os tres elementos a esquerda da tenda da lista da vizinhanca
    % once(
    % (select_true((N,O), Temp3Cima, Temp1Esquerda)) %se tambem esta na primeira linha
    % ; 
    % select_true((N,O), Viz, Temp1Esquerda) %caso contrario
    % ),

    % write(saiu), nl, 
    % select_true((L,O), Temp1Esquerda, Temp2Esquerda),
    % select_true((S,O), Temp2Esquerda, Temp3Esquerda),
    % write(Temp3Esquerda), nl


    % );
    % (write(c), nl,%se estiver na ultima coluna
    % C == Tam,
    % write(ncol),
    % %Remove os tres elementos a direita da tenda da lista da vizinhanca
    % once(
    % (select_true((N,E), Temp3Cima, Temp1Direita)) %se tambem esta na primeira linha
    % ; 
    % select_true((N,E), Viz, Temp1Direita) %caso contrario
    % ),


    % select_true((L,E), Temp1Direita, Temp2Direita),
    % select_true((S,E), Temp2Direita, Temp3Direita),
    % write(Temp3Direita)
    % );
    % (write(d), nl,%se estiver na ultima linha
    % L == Tam,
    
    % %Remove os tres elementos a baixo da tenda da lista da vizinhanca
    % select_true((S,O), Viz, Temp1),
    % select_true((S,C), Temp1, Temp2),
    % select_true((S,E), Temp2, Viz)
    % )
    % )
    % ; write('dasdsadas'), nl,
    
    % %coloca relva nas celulas que restaram.
    
    % maplist(insereObjectoCelula(Tabuleiro, r), Viz),
    % write(depois).
    



%
%
limpaVizinhancas(Puzzle):-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, Tendas, t),
    reuneVizinhancas(Tabuleiro, Tendas, Todas_adj),
    todasCelulas(Tabuleiro, Todas),
    findall(X,(member(X, Todas_adj), member(X, Todas)), Final), % "remove" os elementos que nao pertencem ao tabuleiro,
    maplist(insereObjectoCelula(Tabuleiro, r), Final), !.

%predicado auxiliar que junta numa lista todas as posicoes na vizinhanca de todas as tendas
reuneVizinhancas(Tabuleiro, Tendas, Todas_adj):-
    reuneVizinhancas(Tabuleiro, Tendas, [], Todas_adj), !.

reuneVizinhancas(_, [], X, X) :- !.

reuneVizinhancas(Tabuleiro, [Tenda|R], Todas_adj, New_Todas_adj):-
    % ve tenda a tenda e faz uma lista de todas as posicoes na vizinhanca de tendas (sem repeticoes)
    vizinhancaAlargada(Tenda, Viz), 
    append(Todas_adj, Viz, Adj_Temp),
    sort(Adj_Temp, Adj_Sorted), %remove duplicados
    reuneVizinhancas(Tabuleiro, R, Adj_Sorted, New_Todas_adj), !.


/*unicaHipotese/1
unicaHipotese(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado,
todas as arvores que tinham apenas uma posição livre na sua vizinhança que lhes
permitia ficar ligadas a uma tenda, tem agora uma tenda nessa posição.
*/

%ver adjacentes de todas as arvores : se houver uma arvore que so tem uma adjacente, celula la

unicaHipotese(Puzzle):-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, Arvores, a),
    todasCelulas(Tabuleiro, Todas),
    write(Tabuleiro), nl, 
    verificaArvores(Tabuleiro, Arvores, Todas, Unicas),
    write(Unicas), nl, 
    maplist(insereObjectoCelula(Tabuleiro, t), Unicas), !.


verificaArvores(_, [], X, X):- !.

verificaArvores(Tabuleiro, [(L,C)|R], Todas, Unicas):-
    vizinhanca((L,C), Viz),
    %write(Viz),
    % "remove" os elementos que nao pertencem ao tabuleiro,
    %write((L, C)) ,nl, nl,   
    findall(X, (member(X, Viz), member(X, Todas)), Viz_valida),  
    %write(Viz_valida), nl,
    write(antes), nl,
    write(entrou), nl,
    findall(X, %apenas as celulas que estao mesmo vazias
        (X = (LV, CV), write(X), member((LV, CV), Viz_valida), nth1(LV, Tabuleiro, Linha), nth1(CV, Linha, Cel), celulaVazia(Tabuleiro, (LV, CV)), Cel \= r ), 
    Unicas_temp),
    write(depois), nl,
    write(Unicas_temp),nl,
    length(Unicas_temp, LL),
    (LL > 0, 
    nl, write(Unicas_temp), nl, nl
    );
    append(Unicas, Unicas_temp, Nova),
    verificaArvores(Tabuleiro, R, Todas, Nova), !
    ;
    write(aqui), nl,
    verificaArvores(Tabuleiro, R, Todas,  Unicas), !.

 




