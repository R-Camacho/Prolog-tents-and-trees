/* Functores
Predicados pré-definidos
functor/3, functor(T_C, F, A).
Ex.: 
?- functor(likes(mary, pizza), Name, Arity)  ----> Decomposição
Name = likes,
Arity = 2.
?- functor(X, likes, 2).  -----> Criação
X = likes(_G32, _G33).

arg/3, arg(N, T_C, Arg).   ----> extrair argumentos de estruturas
Ex.:
?- arg(2, buys(jhon, beer), X).
X = beer.
?- arg(2, buys(jhon, beer), beer).
true.
?- X = likes(mary, Y), arg(2, X, pizza).
X = likes(mary, pizza)
Y = pizza. ---> segundo argumento de pizza()

=../2 , =..(T_C, L).   ------> o primeiro elemento da lista L corresponde 
ao functor do termo composto T_C e o resto de L são os argumentos
Ex.:
?- Term =..[likes, mary, pizza].  ----> Criação
Term = likes(many, pizza).
?- likes(mary, pizza) =..=List. ----> Decomposição
List = [likes, mary, pizza].
*/

%Functores
%1.todos/2. (all)
par(X) :- 
    Y is X mod 2, 
    Y == 0. 

todos(_, []):- !.

todos(Pred, [P|R]) :-
    Lit =..[Pred, P],
    Lit, 
    todos(Pred, R).


%2. algum/2. (any)

%algum(_, []) :- !.

algum(Pred, [P|_]) :-
    Lit =..[Pred, P],
    Lit, !.

algum(Pred, [_|R]) :-
    algum(Pred, R).

%3. quantos/3

% quantos(Pred, [P|R], N) :-
%     Lit =..[Pred, P], 
%     Lit,
%     Y is N + 1,
%     quantos(Pred, R, Y).

% quantos(Pred, [_|R], N) :-
%     quantos(Pred, R, N).


%listas
%repete_el/3

unifica(X, Y):-
    X = Y.

repete_el(El, N, L):-
    length(L, N),
    maplist(unifica(El), L).


/*Implemente o predicado substitui_arg/4, tal que substitui_arg(T_c, Arg, Novo_Arg,
Novo_T_c), em que T_c é um termo composto, significa que Novo_T_c é um termo composto 
obtido de T_c substituindo os argumentos que sejam iguais a Arg por Novo_Arg. Por
exemplo,
?- substitui_arg(f([a,b,c],9,9), [a,b,c], abc, Novo_T_c).
Novo_T_c = f(abc, 9, 9).
?- substitui_arg(f([a,b,c],9,9), 9, 99, Novo_T_c).
Novo_T_c = f([a, b, c], 99, 99).
?- substitui_arg(f([a,b,c],9,9), 1, 99, Novo_T_c).
Novo_T_c = f([a, b, c], 9, 9).
*/

%Auxiliares
substitui_el_list(L1, El, Novo_El, L2):-
    maplist(subst(El, Novo_El), L1, L2).

subst(El, NovoEl, X, NovoEl):-
    El == X.

subst(El, NovoEl, X, NovoEl):-
    El \= X.



%------------

substitui_arg(T_c, Arg, Novo_Arg, Novo_T_c):-
    T_c=..[X| Args], 
    substitui_el_list(Args, Arg, Novo_Arg, L2), %L2 é a nova lista de argumentos
    Novo_T_c =..[X | L2].
