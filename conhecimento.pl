- op( 900,xfy,'::' ).
%%ADICIONAR DYNAMIC A TODOS QUE PODEREM SER MODIFICADOS
:- dynamic ecomenda/5.
:- dynamic entrega/5.
:- dynamic review/4.
%
evolucao( Termo ) :-
    findall( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ),!.

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

involucao( Termo ) :-
    findall( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%invariantes

+ecomenda(Nome/ID,_,_,_,_)::(findall(Nome/ID,(ecomenda(Nome/ID,Rua,_,_,_),rua(Rua,_)),LR),
                                length(LR,L),
                                L==1).

+ecomenda(_,_,_,_,_)::(findall(Peso/Preco/Tempo,(ecomenda(_,_,Peso,Preco,Tempo),(Peso>100;Peso<0;Preco<0;Tempo<0)),LR),
                        length(LR,L),
                        L==0).

+entrega(_,Nome/ID,_,_,_)::(findall(Nome/ID,(ecomenda(Nome/ID,_,_,_,_)),LR),
                                length(LR,L),
                                L==1).

+entrega(_,Nome/ID,_,_,_)::(findall(Nome/ID,(entrega(_,Nome/ID,_,_,_)),LR),
                                length(LR,L),
                                L==1).

+review(Nome1/ID1,Nome/ID,_,_)::(findall(Nome/ID,(entrega(Nome1/ID1,Nome/ID,_,_,_)),LR),
                                length(LR,L),
                                L==1).

+review(_,Nome/ID,_,_)::(findall(Nome/ID,(review(_,Nome/ID,_,_)),LR),
                                length(LR,L),
                                L==1).

%---peso maximo que a empresa consegue entregar
pesoMaximo(100).
%----meio de transporte(meio,peso max,velocidade).
transporte(bicicleta,5,10).
transporte(moto,20,35).
transporte(carro,100,25).
%---penalidade da velocidade
penalidade(bicicleta,0.7).
penalidade(moto,0.5).
penalidade(carro,0.1).
%---ecologico(meioTransporte, Grau > mais ecoclogico).
ecologico(bicicleta,3).
ecologico(moto,2).
ecologico(carro,1).
%---estafeta(nome/id).
estafeta(antonio/0).
estafeta(joao/0).
%---ecomenda(nome/id,rua,peso,preco,tempo max de entrega em h (0 e imediato)).
ecomenda(televisao/0,armandoR,15,780,0).
ecomenda(televisao/1,joaoR,10,500,12).
ecomenda(televisao/2,mariaR,10,460,16).
ecomenda(pc/0,antonioR,2,780,24).
ecomenda(pc/1,antonioR,8,780,24).
%---entrega realizada(cliente/id,Nome/.ecomenda, estafeta/id, meio, dia/mes/ano).
entrega(pedro/0,televisao/0, antonio/0, mota, 12/03/2001).
entrega(manuel/0,televisao/1, antonio/0, mota, 12/03/2001).
entrega(manuel/0,pc/1, antonio/0, mota, 12/03/2001).
entrega(vitor/0,televisao/2, antonio/0, mota, 12/03/2001).
entrega(pedro/0,pc/0, joao/0, bicicleta, 12/04/2002).
%---review(cliente/id,ecomenda/id,classificação,comentario).
review(pedro/0,televisao/0,4,"bueno").
review(manuel/0,televisao/1,1,"meh").

%---precoExtra(peso,meio de transporte, tempo de transporte,total)
precoExtra(Peso,Transporte,Tempo,Total) :- precoPeso(Peso,PrecoPeso), precoTransporte(Transporte,PrecoTransporte), precoTempo(Tempo,PrecoTempo),
                                           Total is PrecoPeso + PrecoTempo + PrecoTransporte.

precoPeso(Peso,5) :- Peso < 5, !.
precoPeso(Peso,10) :- Peso < 20, !.
precoPeso(Peso,20) :- Peso < 50, !.
precoPeso(Peso,30) :- Peso < 100, !.

precoTempo(0,30) :- !.
precoTempo(Tempo,15) :- Tempo < 2 , !.
precoTempo(Tempo,10) :- Tempo < 6 , !.
precoTempo(_,5)  :- !.

precoTransporte(bicicleta,5).
precoTransporte(moto,15).
precoTransporte(carro,20).

%---Mapa
%----sede(localizacão da sede).
sede(joaoR).
%---ruas de um ciadade(nome da rua,fregesia).
rua(antonioR,sVictor).
rua(joaoR,sVictor).
rua(mariaR,sVitor).
rua(afonsoR,sVicente).
rua(tiagoR,sVicente).
rua(diogoR,gualtar).
rua(armandoR,gualtar).
rua(semilhaR,gualtar).
%---distancia em linha reta a sede
estimativa(joaoR,0).
estimativa(antonioR,1.4).
estimativa(afonsoR,1).
estimativa(mariaR,2.5).
estimativa(tiagoR,1.6).
estimativa(diogoR,2.3).
estimativa(armandoR,3.7).
estimativa(semilhaR,2.1).
%---ruasAdj(rua,rua) ,depois para ver as freg -> rua(Nome,freg)
ruasAdj(joaoR,antonioR,1.4).
ruasAdj(joaoR,afonsoR,1).
ruasAdj(antonioR,mariaR,2).
ruasAdj(antonioR,afonsoR,1.2).
ruasAdj(mariaR,tiagoR,1.7).
ruasAdj(tiagoR,afonsoR,0.5).
ruasAdj(tiagoR,diogoR,2.9).
ruasAdj(tiagoR,semilhaR,1.5).
ruasAdj(semilhaR,armandoR,1.4).
ruasAdj(diogoR,armandoR,1.4).
ruasAdj(afonsoR,diogoR,1.9).
