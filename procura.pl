
:- ensure_loaded(conhecimento).

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

%%AUXILIARES

Auxiliares
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
