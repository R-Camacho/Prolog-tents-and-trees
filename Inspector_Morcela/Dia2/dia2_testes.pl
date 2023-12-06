/*

Teste do predicado do momento 2

*/

:- MensagensDia2 = [[fail,hi],[o,o],[ola,sea],[o,45],[ola,ola],[d,5],[d,d],[fail,bye],[ola,o],
[y,e],[d,e],[y,y],[y,a],[o,e],[ola,y],[y,ola],[o,bye],[y,fail],[ola,d],[y,hello],
[fail,e],[fail,glory],[ola,e],[bye,a]],
SeqFonte = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
SeqAlvo = [o, d, e, y, ola, bye, fail, 5, a, j, 0, 45, sea, hello, hi, glory], 
maplist(descodificaDia2(SeqFonte, SeqAlvo), MensagensDia2, MensagemDecifradas),
nl, nl, writeln('Mensagens Decifradas (dia 2):'),
maplist(writeln, MensagemDecifradas).
