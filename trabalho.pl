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
%---estafeta(nome e id).
estafeta(antonio0).
%---ecomenda(nome e id,rua,peso,preco,tempo max de entrega em h (0 e imediato)).
ecomenda(televisao0,antonioR,10,780,24).
%---entrega realizada(Nome da ecomenda, estafeta, meio, dia/mes/ano).
entrega(televisao0, antonio0, bicicleta, 12/03/2001).

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