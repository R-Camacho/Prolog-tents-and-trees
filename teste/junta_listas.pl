% junta duas listas

junta([], L, L). % uma das listas (primeira) vazia
junta([P|R], L1, [P|L2]) :- junta(R, L1, L2).
