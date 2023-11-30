% Determina se um determinado elemento pertence a uma lista

membro(E, [E|_]).
membro(E, [_|R]) :- membro(E, R). 
