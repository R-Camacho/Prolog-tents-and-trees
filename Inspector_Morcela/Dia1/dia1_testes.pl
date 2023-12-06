/*

Implementa o predicado eliminaNumeros(L1, L2).

Supondo que o ficheiro em que implementaste eliminaNumeros(L1, L2) se chama FICH.pl, vai 
para a directoria em que esta esse ficheiro e este e entra no Prolog. Se o ficheiro nao estiver 
carregado, faz:

?- ['FICH.pl'].

Se nao tiveres erros, para testar, faz:

?- ['dia1_testes.pl'].

Estudaremos o maplist na semana que vem.

*/

:- MensagensDia1 = [
[a, 4, m, 77, a, 2, 1, 4, t, r, 4, 6, i, 1, 2, z, s, 4, 66, 99, e, 8, 17, r, 88, a, 17, c, h, a, 889, 1, 2, v, e],
[s, 6, 7, 8, 1, e, t, 67, 54, 1, 2, e, 8, p, 9, o, 2, r, d, 43, 41, e, z, 90, 91, a, s, 76, 87, s, e, 65, 66, i, s],
[5, f, 7, 9, 8, r, 8, a, 99, s, 1, e, 12, s, 13, 44, 1, s, 7, 18, e, 1, 8, 9, m, s, e, 1, 7, 2, 15, n, t, 19, 18, 33, 2, i, d, o],
[7, 8, p, e, r, 7, 8, 9, u, c, 6, 99, 13, 15, a, n, e, 18, 19, g, r, 66, 22, 14, a, e, 19, b, o, 88, n, e],
[o, 7, 8, c, 1, 2, u, 9, 10, 11, l, o, 12, 17, 18, s, e, 19, 20, 11, b, o, 1, 2, 7, 10, t, a, 11, 34, s, 87, n, 2, e, 2, 3, v, e],
[1, 2, 3, 9, f, 8, r, a, 9, s, 10, e, 11, s, 65, c, 45, o, 88, m, 33, v, e, 23, r, 21, 22, d, a, 2, 3, 76, d, e],
[76, t, 88, u, 98, 1, 2, d, o, 3, 5, 66, a, p, 78, 99, o, s, 1, 2, 5, t, o, s, 76, 55, 43, r, o, 1, 4, 5, 78, u, b, o]],
maplist(eliminaNumeros, MensagensDia1, MensagemDecifradas), 
writeln('Mensagens Decifradas (dia 1):'),
maplist(writeln, MensagemDecifradas).
