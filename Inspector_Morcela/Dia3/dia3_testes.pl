
/*

Teste dos predicados do dia 3

*/

:- SeqFonte = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
SeqAlvo = [o, d, e, y, ola, bye, fail, 5, a, j, 0, 45, sea, hello, hi, glory],
programa1Dia3(SeqFonte, SeqAlvo, ListaParesCodigo),
nl, nl, writeln('Predicado 1 (dia 3):'),
maplist(writeln, ListaParesCodigo), 
MensagensDia2 = [[fail,hi],[o,o],[ola,sea],[o,45],[ola,ola],[d,5],[d,d],[fail,bye],[ola,o],
[y,e],[d,e],[y,y],[y,a],[o,e],[ola,y],[y,ola],[o,bye],[y,fail],[ola,d],[y,hello],
[fail,e],[fail,glory],[ola,e],[bye,a]],
maplist(programa2Dia3(ListaParesCodigo), MensagensDia2, MensagensDescodificadas),
nl, nl, writeln('Mensagens Decifradas (dia 3) (devera ser igual ao teste do predicado de dia 2):'),
maplist(writeln, MensagensDescodificadas).