:- set_prolog_flag(answer_write_options,[max_depth(0)]).

/*--------------------------------------------------------------------------------
Dia 5: encontraSuspeitos(PessoasBanco, CaracteristicasCriminoso, Suspeitos) e verdade
se Suspeitos for o conjunto de pessoas de PessoasBanco que tem as caracteristicas de 
CaracteristicasCriminoso.
----------------------------------------------------------------------------------*/

encontraSuspeitos(PessoasBanco, CaracteristicasCriminoso, Suspeitos) :- 
      findall(Nome, (member(pessoa(Nome, CaracteristicasPessoa), PessoasBanco), 
                     subset(CaracteristicasCriminoso,CaracteristicasPessoa)),
                     Suspeitos).
