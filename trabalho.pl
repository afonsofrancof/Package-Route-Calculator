%----meio de transporte(meio,peso max,velocidade).
transporte(bicicleta,5,10).
transporte(moto,20,35).
transporte(carro,100,25).
%---ecologico(meioTransporte, Grau > mais ecoclogico).
ecologico(bicicleta,3).
ecologico(moto,2).
ecologico(carro,1).
%---estafeta(nome,classificação,localizacao,lista de entregas,entregas realizadas a tempo,entregas realizadas).
estafeta(antonio,0,joao,[],0,0).
%---ecomenda(nome,rua,peso,preco,tempo max de entrega em h (0 e imediato)).
ecomenda(televisao,antonioR,10,780,24).
%---ruas de um ciadade(nome da rua,fregesia).
rua(antonioR,sVictor).
rua(joaoR,sVictor).
rua(mariaR,sVicente).
rua(tiagoR,sVicente).
%---V1 dos mapas
%---ruasAdj(rua,rua).
ruasAdj(antonioR,joaoR).
ruasAdj(joaoR,mariaR).
ruasAdj(tiagoR,mariaR).
ruasAdj(antonioR,tiagoR).
%---fregAdj(freg,freg,[ruasAdj]).
fregAdj(sVictor,sVicente, [
    ruasAdj(joaoR,mariaR),
    ruasAdj(antonioR,tiagoR),
]).

%---V2 dos mapas
adjcentes(sVictor,sVictor,[
    ruasAdj(antonioR,joaoR)
]).
adjcentes(sVictor,sVicente, [
    ruasAdj(joaoR,mariaR),
    ruasAdj(antonioR,tiagoR)
]).
adjcentes(sVicente,sVicente, [
    ruasAdj(tiagoR,mariaR)
]).


