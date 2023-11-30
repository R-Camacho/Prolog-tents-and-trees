%obtem o ultimo elemento de uma lista

ultimo([X], X).
ultimo([_|R], X) :- ultimo(R, X).