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
ecomenda(pc/0,antonioR,10,780,24).
%---entrega realizada(Nome/ecomenda, estafeta/id, meio, dia/mes/ano).
entrega(televisao/0, antonio/0, bicicleta, 12/03/2001).
entrega(pc/0, joao/0, bicicleta, 12/04/2002).
%---reivew(cliente/id,ecomenda/id,classificação,comentario).
review(pedro/0,televisao/0,4,"bueno").

%---Exercicios
entregaTotais(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                    (entrega(Ecomenda,_,_,Dia/Mes/Ano),
                                                    entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                    LE), length(LE,Total).

entregaPorMeio(Meio,Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                    (entrega(Ecomenda,_,Meio,Dia/Mes/Ano),
                                                    entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                    LE), length(LE,Total).


%---Func Aux
entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- emDias(Dia/Mes/Ano,K), 
                                                        emDias(DiaI/MesI/AnoI,KIncial), 
                                                        emDias(DiaF/MesF/AnoF,KFinal), 
                                                        K =< KFinal, K >= KIncial.
emDias(Dia/Mes/Ano,Dias) :- A is Ano * 365, M is 30 * Mes, Dias is Dia + A + M. 
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