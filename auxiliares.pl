:- ensure_loaded(conhecimento).
:- ensure_loaded(procura).


%Auxiliares
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

