:- op( 900,xfy,'::' ).
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

%---Exercicios

%1
first([H|_],H).

estafetaEcologico(MelhorEstafeta/Id) :- findall(Estafeta/Id/Meio ,(entrega(_,_,Estafeta/Id,Transporte,_),ecologico(Transporte,Meio)),Est),
                                                        construirId0(Est,[],List),
                                                        first(List,F),
                                                        maiorEcologiaList(List,F,MelhorEstafeta/Id/_).



construirId0([],List,List).
construirId0([Estafeta/Id/Value|X],L,List):- \+member(Estafeta/Id/_/_,L) -> construirId0(X,[Estafeta/Id/Value/0|L],List) ; nth0(Index,L,Estafeta/Id/Valor/Times),
                                        H is Valor*Times,A is H+Value,B is Times+1, C is A/B,
                                        replace(L,Index,Estafeta/Id/C/B,F),
                                        construirId0(X,F,List).
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):-
        I > -1,
        NI is I-1,
        replace(T, NI, X, R), !.
replace(L, _, _, L).

maiorEcologiaList([],MelhorEstafeta/MelhorId/MelhorValor,MelhorEstafeta/MelhorId/MelhorValor).
maiorEcologiaList([Estafeta/Id/Valor/_|X],MelhorEstafeta/MelhorId/MelhorValor,Y) :- Valor>MelhorValor ->
                                                                                                        maiorEcologiaList(X,Estafeta/Id/Valor,Y);
                                                                                                        maiorEcologiaList(X,MelhorEstafeta/MelhorId/MelhorValor,Y).


%2
entregasACliente(Cliente,Ecomendas,LE) :- findall(Estafeta, (entrega(Cliente,Ecomenda,Estafeta,_,_),
                                          member(Ecomenda,Ecomendas)), LE).

%3
clientesServidosPorEstafeta(ClientesSemRepetidos,Estafeta/Id) :- findall(Cliente, (entrega(Cliente,_,Estafeta/Id,_,_)),Clientes),removerClientesRepetidos(Clientes,[],ClientesSemRepetidos).

%4
calcularValorDia(Valor,Dia/Mes/Ano):- findall(Preco , (ecomenda(Nome/Id,_,Peso,_,Tempo),
                                                      entrega(_,Nome/Id,_,Transporte,Dia/Mes/Ano),precoExtra(Peso,Transporte,Tempo,Preco)),
                                                      LP), sumLista(LP,Valor).

%5
ruaMaiorVolumeEntregas(Zona) :- findall(Rua ,(ecomenda(Nome/Id,Rua,_,_,_),entrega(_,Nome/Id,_,_,_)),LR),maisRepetido(LR,Zona,_).

fregMaiorVolumeEntregas(Zona) :- findall(Freg ,
                                        (ecomenda(Nome/Id,Rua,_,_,_),entrega(_,Nome/Id,_,_,_),rua(Rua,Freg)),LF),
                                        maisRepetido(LF,Zona,_).

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
entregasTodosEstafetas(Entregas,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall( (Estafeta,Total),
                                                                  (estafeta(Estafeta), entregaPorEstafeta(Estafeta,Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),Entregas).

entregaPorEstafeta(Estafeta,Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                           (entrega(_,Ecomenda,Estafeta,_,Dia/Mes/Ano),
                                                           entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                           LE), length(LE,Total).
%para alterar , 2 listas com if  -> ;
%9
entregaNoTempo(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF):- findall(Ecomenda,
                                                      (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                                      entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF)),
                                                      LE),length(LE,Total).

naoEntregeNoTempo(Total,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- findall(Ecomenda,
                                                          (entrega(_,Ecomenda,_,_,Dia/Mes/Ano),
                                                          not(entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF))),
                                                          LE),length(LE,Total).
%10
pesoTotalEstDia(Entregas,Dia/Mes/Ano):- findall((Estafeta,Total),
                                        (estafeta(Estafeta),pesoPorEstafetaDia(Estafeta,Total,Dia/Mes/Ano)),Entregas).

pesoPorEstafetaDia(Estafeta,Total,Dia/Mes/Ano) :- findall(Peso,
                                                  (entrega(_,Ecomenda,Estafeta,_,Dia/Mes/Ano),
                                                  ecomenda(Ecomenda,_,Peso,_,_)),
                                                  LE), sumLista(LE,Total).

%

fregMaiorPesoEntregas(Zona) :- findall(Freg/PesoTotal,(rua(_,Freg),
                                                     findall(Peso,(ecomenda(Nome/ID,Rua,Peso,_,_),entrega(_,Nome/ID,_,_,_),rua(Rua,Freg)) ,LP),
                                                     sumLista(LP,PesoTotal)),LT), maiorPeso(LT,Zona,_).

ruaMaiorPesoEntregas(Zona) :- findall(Rua/PesoTotal,(rua(Rua,_),
                                                     findall(Peso,(entrega(_,D/I,_,_,_),ecomenda(D/I,Rua,Peso,_,_)),LP),
                                                     sumLista(LP,PesoTotal)),LT), maiorPeso(LT,Zona,_).


%%Algoritmos de procura
%aestrela
resolve_aestrela(Nodo, Caminho/Custo) :-
	estimativa(Nodo, Estima),
	aestrela([[Nodo]/0/Estima], Caminho/Custo/_).

aestrela(Caminhos, Caminho) :-
	obtem_melhor(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,
	sede(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
	obtem_melhor(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho).	

obtem_melhor([Caminho], Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
	           obtem_melhor(Caminhos, MelhorCaminho).

expande_aestrela(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).


%gulosa
resolve_gulosa(Nodo, Caminho/Custo) :-
	estimativa(Nodo, Estima),
	agulosa([[Nodo]/0/Estima], Caminho/Custo/_).

agulosa(Caminhos, Caminho) :-
	obtem_melhor_g(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,
	sede(Nodo).
agulosa(Caminhos, SolucaoCaminho) :-
	obtem_melhor_g(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_gulosa(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    agulosa(NovoCaminhos, SolucaoCaminho).		


obtem_melhor_g([Caminho], Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !,
	obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).	
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_g(Caminhos, MelhorCaminho).

expande_gulosa(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).	

adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	adjacente(Nodo, ProxNodo, PassoCusto),
	\+member(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estimativa(ProxNodo, Est).

% depth first
resolve_pp_c(Destino, Nodo, [Nodo|Caminho], C) :-
	            profundidadeprimeiro(Destino, Nodo, [Nodo], Caminho, C).

profundidadeprimeiro(Destino, Destino, _, [], 0).
profundidadeprimeiro(Destino, Nodo, Historico, [ProxNodo|Caminho], C) :-
                adjacente(Nodo, ProxNodo, C1),
                not(member(ProxNodo, Historico)),
                profundidadeprimeiro(Destino, ProxNodo, [ProxNodo|Historico], Caminho, C2), 
                C is C1 + C2.

% breadth first
bfs(Orig, Dest, Cam):- bfs2(Dest,[[Orig]],Cam).
bfs2(Dest,[[Dest|T]|_],Cam):- reverse([Dest|T],Cam). 
bfs2(Dest,[LA|Outros],Cam):- 
                        LA=[Act|_],
                        findall([X|LA],(Dest\==Act,adjacente(Act,X,_),\+member(X/_,LA)),Novos),
                        append(Outros,Novos,Todos),
                        bfs2(Dest,Todos,Cam).

% Iterative depth first
resolve_iter(Destino, Nodo, [Nodo|Caminho]) :- iter_depth_call(Destino,Nodo,Caminho,0).
iter_depth_call(Destino, Nodo, Caminho, Depth) :-
	            iter_depth(Destino, Nodo, [Nodo], Caminho, Depth) -> !;
                Z is Depth + 1,
                iter_depth_call(Destino, Nodo, Caminho, Z).

iter_depth(Destino, Destino, _, [], 1).
iter_depth(Destino, Nodo, Historico, [ProxNodo|Caminho], Depth) :-
                adjacente(Nodo, ProxNodo, _),
                not(member(ProxNodo, Historico)),
                Z is Depth - 1,
                Z > 0,
                iter_depth(Destino, ProxNodo, [ProxNodo|Historico], Caminho, Z).


%% Auxiliares
entreData(Dia/Mes/Ano,DiaI/MesI/AnoI,DiaF/MesF/AnoF) :- date_time_stamp(date(Ano,Mes,Dia),X),
                                                    date_time_stamp(date(AnoI,MesI,DiaI),Y),
                                                    date_time_stamp(date(AnoF,MesF,DiaF),Z),
                                                    X<Z,
                                                    X>Y.
removerClientesRepetidos([],List,List).
removerClientesRepetidos([Cliente|X],Empty,List):- member(Cliente,Empty) -> removerClientesRepetidos(X,Empty,List) ; removerClientesRepetidos(X,[Cliente|Empty],List).

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

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

adjacente(Nodo, ProxNodo, C) :- 
	ruasAdj(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- 
	ruasAdj(ProxNodo, Nodo, C).

distancia([Rua|Prox],Total) :- 
                        nth0(0,Prox,Elem),
                        distancia(Prox, DistProx), 
                        adjacente(Rua, Elem, Distancia),
                        Total is Distancia + DistProx.
distancia([_],0).

velocidadeTransporte(Veiculo,Peso,VelocidadePenalizada):- transporte(Veiculo,_,Velocidade),
                                                penalidade(Veiculo,Penalidade),Z is Penalidade*Peso,
                                                Z =< Velocidade -> VelocidadePenalizada is Velocidade-Z ; VelocidadePenalizada is 0.

transporteMaisRapido([Transporte/Velocidade] ,Velocidade,Transporte).
transporteMaisRapido([TransporteAtual/VelocidadeAtual|Next],VelocidadeMaxAtual,TransporteMaxAtual) :- transporteMaisRapido(Next,VelocidadeMax,TransporteMax),
                                                                                  (VelocidadeAtual > VelocidadeMax ->
                                                                                  TransporteMaxAtual = TransporteAtual,
                                                                                  VelocidadeMaxAtual is VelocidadeAtual ;                                                                            
                                                                                   VelocidadeMaxAtual is VelocidadeMax,
                                                                                   TransporteMaxAtual = TransporteMax),!.


veiculosPossiveis([Veiculo/Velocidade],Distancia,Tempo,LV):- TempoV is Distancia/Velocidade,TempoV < Tempo -> LV = [Veiculo] ; LV = [],!.
veiculosPossiveis([Veiculo/Velocidade|Prox],Distancia,Tempo,LV):- veiculosPossiveis(Prox,Distancia,Tempo,ListVeiculoRecebida),
                                                                TempoV is Distancia/Velocidade,
                                                                TempoV < Tempo -> LV = [Veiculo|ListVeiculoRecebida] ; LV = ListVeiculoRecebida.

veiculoMaisEcologico([Veiculo], Veiculo,Ecologia):- ecologico(Veiculo,Ecologia).
veiculoMaisEcologico([Veiculo|Prox], VeiculoE,EcologiaE) :- veiculoMaisEcologico(Prox,VeiculoAux,EcologiaAux),
                                                                            ecologico(Veiculo,Ecologia),
                                                                            (Ecologia > EcologiaAux -> 
                                                                            EcologiaE = Ecologia,
                                                                            VeiculoE = Veiculo;
                                                                            EcologiaE = EcologiaAux,
                                                                            VeiculoE = VeiculoAux),!.

selecionaMelhorTransporte([Veiculo/Velocidade|Prox], Distancia, Tempo, Veiculo) :- selecionaMelhorTransporte(Prox, Distancia, Transporte),
                                                                                   ecologico(Veiculo,VeiculoGrau), ecologico(Transporte,TransporteGrau),
                                                                                   VeiculoGrau > TransporteGrau , TempoTotal is Distancia / Velocidade, TempoTotal < Tempo, !.
selecionaMelhorTransporte([_/_|Prox], Distancia, Transporte) :- selecionaMelhorTransporte(Prox,Distancia,Transporte).
selecionaMelhorTransporte([Veiculo/_], _, Veiculo).

retiraDistancias([Rua/_|Prox], [Rua|Resultado]) :- retiraDistancias(Prox,Resultado).
retiraDistancias([],[]).

maiorPeso([],Zona,0) :- sede(Zona).
maiorPeso([Zona/PesoTotal|Resto],Zona,PesoTotal) :- maiorPeso(Resto,_,PesoFinal), PesoTotal > PesoFinal , !.
maiorPeso([_ | Resto],Zona,PesoTotal) :- maiorPeso(Resto,Zona,PesoTotal). 

ruaLista([],[]).
ruaLista([Encomenda/ID|T],[Rua|RuaLista]) :- ruaLista(T,RuaLista), ecomenda(Encomenda/ID,Rua,_,_,_), not(member(Rua,RuaLista)), !.
ruaLista([_|T],RuaLista) :- ruaLista(T,RuaLista).

removeRepetidos([H|T], Caminho,[H|SR]) :- removeRepetidos(T, Caminho, SR),
                                                not(member(H,Caminho)),!.
removeRepetidos([_|T],Caminho,SR):- removeRepetidos(T,Caminho,SR).
removeRepetidos([],_,[]).

takeHead([],[]).
takeHead([_|T],T).

%%
recomendacaoAlgoritmo(Entrega/ID,Algoritmo,Transporte/Distancia/CaminhoTotal) :- statistics(runtime,[Start|_]),
                                                                                ecomenda(Entrega/ID,Destino,Peso,_,Tempo),
                                                                                caminho(Algoritmo,Destino,Distancia,Caminho),
                                                                                reverse(Caminho,[_|Tail]), append(Caminho,Tail,CaminhoTotal),
                                                                                findall(Veiculo/VelocidadePenalizada, 
                                                                                (transporte(Veiculo,Max,_), Peso =< Max, velocidadeTransporte(Veiculo,Peso,VelocidadePenalizada)), LV),
                                                                                veiculosPossiveis(LV,Distancia,Tempo,ListaPossiveis),
                                                                                length(ListaPossiveis,LengthLista),
                                                                                (LengthLista =\= 0 ->
                                                                                veiculoMaisEcologico(ListaPossiveis,Transporte,_);
                                                                                transporteMaisRapido(LV,_,Transporte)),
                                                                                statistics(runtime,[Stop|_]),
                                                                                Time is Stop - Start,
                                                                                Runtime is Time * 1000,
                                                                                nl,write('Time: '),write(Runtime), write(' ms'),!.

recomendacaoVariasAlgoritmo(Ecomendas,Transporte/Distancia/Caminho) :- predsort(cmpEncomendaTempo,Ecomendas,Sorted),
                                                                        ruaLista(Sorted,Destinos),
                                                                        fullyCaminho(Destinos,Distancia,Caminho).
                                                                        %findall(P,(ecomenda(E,_,P,_,_),member(E,Ecomendas)),LP), sumLista(LP,Peso),
                                                                        %findall(Veiculo/VelocidadePenalizada, 
                                                                        %(transporte(Veiculo,Max,_), Peso =< Max, velocidadeTransporte(Veiculo,Peso,VelocidadePenalizada)), LV),
                                                                        %veiculosPossiveis(LV,Distancia,Tempo,ListaPossiveis),
                                                                        %length(ListaPossiveis,LengthLista),
                                                                        %(LengthLista =\= 0 -> veiculoMaisEcologico(ListaPossiveis,Transporte,_);
                                                                        %transporteMaisRapido(LV,_,Transporte)).                                                             


fullyCaminho(Destinos,Distancia,Caminho) :- sede(Rua),fullyCaminhoAux(Rua,Destinos,CaminhoFully), last(CaminhoFully, Last),
                                            resolve_iter(Rua,Last,CaminhoFinal), takeHead(CaminhoFinal,Tail),
                                            append(CaminhoFully,Tail,Caminho), distancia(Caminho,Distancia).

fullyCaminhoAux(_,[],[]).
fullyCaminhoAux(Saida,Destinos,Caminho) :- [H|_] = Destinos, resolve_iter(H,Saida,CaminhoPrimeiro),
                                                    removeRepetidos(Destinos,CaminhoPrimeiro,NewDestinos),
                                                    fullyCaminhoAux(H,NewDestinos,CaminhoSegundo), takeHead(CaminhoSegundo,Tail), 
                                                    append(CaminhoPrimeiro,Tail,Caminho).                                                       

caminho(df,Destino,Distancia,Caminho) :- sede(Rua),resolve_pp_c(Destino,Rua,Caminho,Distancia).
caminho(bf,Destino,Distancia,Caminho) :- sede(Rua),bfs(Rua,Destino,Caminho),distancia(Caminho,Distancia).
caminho(dfi,Destino,Distancia,Caminho) :- sede(Rua),resolve_iter(Destino,Rua,Caminho),distancia(Caminho,Distancia).
caminho(aestrela,Destino,Distancia,Caminho) :- resolve_aestrela(Destino,Caminho/Distancia).
caminho(gulosa,Destino,Distancia,Caminho) :- resolve_gulosa(Destino,Caminho/Distancia).

cmpEncomendaTempo(<,Ecomenda,Ecomenda1) :- ecomenda(Ecomenda,_,_,_,Tempo), ecomenda(Ecomenda1,_,_,_,Tempo1), Tempo < Tempo1.
cmpEncomendaTempo(>,Ecomenda,Ecomenda1) :- ecomenda(Ecomenda,_,_,_,Tempo), ecomenda(Ecomenda1,_,_,_,Tempo1), Tempo >= Tempo1.