%----sede(localizaão da sede).
sede(joaoR).
%----meio de transporte(meio,peso max,velocidade).
transporte(bicicleta,5,10).
transporte(moto,20,35).
transporte(carro,100,25).
%---ecologico(meioTransporte, Grau > mais ecoclogico).
ecologico(bicicleta,3).
ecologico(moto,2).
ecologico(carro,1).
%---estafeta(nome/id).
estafeta(antonio/0).
estafeta(joao/0).
%---ecomenda(nome/id,rua,peso,preco,tempo max de entrega em h (0 e imediato)).
ecomenda(televisao/0,antonioR,10,780,24).
ecomenda(televisao/1,joaoR,10,500,12).
ecomenda(televisao/2,mariaR,10,460,16).
ecomenda(pc/0,antonioR,10,780,24).
%---entrega realizada(cliente/id,Nome/ecomenda, estafeta/id, meio, dia/mes/ano).
entrega(pedro/0,televisao/0, antonio/0, bicicleta, 12/03/2001).
entrega(manuel/0,televisao/1, antonio/0, bicicleta, 12/03/2001).
entrega(vitor/0,televisao/2, antonio/0, bicicleta, 12/03/2001).
entrega(pedro/0,pc/0, joao/0, bicicleta, 12/04/2002).
%---review(cliente/id,ecomenda/id,classificação,comentario).
review(pedro/0,televisao/0,4,"bueno").
review(manuel/0,televisao/1,1,"meh").

%---Exercicios

%1
estafetaEcologico(EstafetaReturn) :- findall(Estafeta/Id/Valor ,(entrega(_,_,Estafeta/Id,Meio,_),ecologico(Meio,Valor)),Est).
%Falta somar os gajos da lista ou pensar noutro metodo.                         
%Listas deste tipo [antonio/0/3,antonio/0/2,joao/0/1] , em que cada item corresponde a 1 valor de ecologia, por isso cada estafeta em 1 
%item na lista por entrega que fez. Basicamente, até agora, o que a estafetaEcologico faz e transformar os veiculos em valores ecologicos.

%2
entregasACliente(Cliente,Ecomendas,LE) :- findall(Estafeta, (entrega(Cliente,Ecomenda,Estafeta,_,_),
                                          member(Ecomenda,Ecomendas)), LE).

%3
clientesServidosPorEstafeta(Clientes,Estafeta/Id) :- findall(Cliente, (entrega(Cliente,_,Estafeta/Id,_,_)),Clientes).

%4
calcularValorDia(Valor,Dia/Mes/Ano):- findall(Preco , (ecomenda(Nome/Id,_,_,Preco,_),entrega(_,Nome/Id,_,_,Dia/Mes/Ano)),LP), sumLista(LP,Valor).

%5
zonaMaiorVolumeEntregas(Zona) :- findall(Rua ,(ecomenda(Nome/Id,Rua,_,_,_),entrega(_,Nome/Id,_,_,_)),LR),maisRepetido(LR,Zona,_).

%6
classificacaoEstafeta(Valor,Estafeta/IdE) :- findall(Classificacao, (entrega(_,Nome/Id,Estafeta/IdE,_,_),review(_,Nome/Id,Classificacao,_)),LC),
                                            sumLista(LC,LP),
                                            length(LC,LengthClass),
                                            Valor is LP / LengthClass.

%7
entregaPorMeio(Meio,Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                    (entrega(_,Ecomenda,_,Meio,Dia/Mes/Ano),
                                                    entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                    LE), length(LE,Total).

%8
entregaTotais(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                      (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                                      entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                      LE), length(LE,Total).

%9
entregaNoTempo(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF):- findall(Ecomenda,
                                                      (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                                      entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                      LE),length(LE,Total).

naoentregeNoTempo(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                          (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                                          not(entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF))),
                                                          LE),length(LE,Total).
%10
pesoTotalEstDia(Total,Dia/Mes/Ano):- findall(Peso,
                                     (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                     ecomenda(Ecomenda,_,Peso,_,_)),
                                     LP),sumLista(LP,Total).

%---Func Aux
entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- emDias(Dia/Mes/Ano,K), 
                                                        emDias(DiaI/MesI/AnoI,KIncial), 
                                                        emDias(DiaF/MesF/AnoF,KFinal), 
                                                        K =< KFinal, K >= KIncial.
emDias(Dia/Mes/Ano,Dias) :- A is Ano * 365, M is 30 * Mes, Dias is Dia + A + M. 

maisRepetido([Tipo],Tipo,1).
maisRepetido([X|Tipos],X,QuantosRepetidos) :- maisRepetido(Tipos,_,Q), 
                                              quantosRepetidos([X|Tipos],X,QuantosRepetidos), 
                                              Q < QuantosRepetidos,!.
maisRepetido([_|Tipos],Tipo,Quantos) :- maisRepetido(Tipos,Tipo,Quantos).

quantosRepetidos([],_,0).
quantosRepetidos([Tipo|Tipos],Tipo,Quantos) :- !, quantosRepetidos(Tipos,Tipo,Quantos1), Quantos is Quantos1 + 1.
quantosRepetidos([_|Tipos],Tipo,Quantos) :- quantosRepetidos(Tipos,Tipo,Quantos).

sumLista([],0).
sumLista([X|Y],K):- sumLista(Y,K1), K is X + K1.
%
%---V1 dos mapas
%---ruasAdj(rua,rua).
%ruasAdj(antonioR,joaoR). 
%ruasAdj(joaoR,mariaR).
%ruasAdj(tiagoR,mariaR).
%ruasAdj(antonioR,tiagoR).
%---fregAdj(freg,freg,[ruasAdj]).
%fregAdj(sVictor,sVicente, [
%    ruasAdj(joaoR,mariaR),
%    ruasAdj(antonioR,tiagoR),
%]).
%---V2 dos mapas
%adjcentes(sVictor,sVictor,[
%    ruasAdj(antonioR,joaoR)
%]).
%adjcentes(sVictor,sVicente, [
%    ruasAdj(joaoR,mariaR),
%    ruasAdj(antonioR,tiagoR)
%]).
%adjcentes(sVicente,sVicente, [
%    ruasAdj(tiagoR,mariaR)
%]).

%---V3 dos mapas
%---ruas de um ciadade(nome da rua,fregesia).
rua(antonioR,sVictor).
rua(joaoR,sVictor).
rua(mariaR,sVicente).
rua(tiagoR,sVicente).
%---ruasAdj(rua,rua) ,depois para ver as freg -> rua(Nome,freg)
ruasAdj(antonioR,joaoR).
ruasAdj(joaoR,mariaR).
ruasAdj(tiagoR,mariaR).
ruasAdj(antonioR,tiagoR).