
:- ensure_loaded(conhecimento).
:- ensure_loaded(procura).

recomendacao(E,Transporte/Distancia/Caminho) :- recomendacaoAlgoritmo(E,aestrela,Transporte/Distancia/Caminho).

recomendacaoAlgoritmo(Entrega/ID,Algoritmo,Transporte/Distancia/CaminhoTotal) :- ecomenda(Entrega/ID,Destino,Peso,_,Tempo),
                                                                                caminho(Algoritmo,Destino,Distancia,Caminho),
                                                                                reverse(Caminho,[_|Tail]), append(Caminho,Tail,CaminhoTotal),
                                                                                findall(Veiculo/VelocidadePenalizada, 
                                                                                (transporte(Veiculo,Max,_), Peso =< Max, velocidadeTransporte(Veiculo,Peso,VelocidadePenalizada)), LV),
                                                                                veiculosPossiveis(LV,Distancia,Tempo,ListaPossiveis),
                                                                                length(ListaPossiveis,LengthLista),
                                                                                (LengthLista =\= 0 ->
                                                                                veiculoMaisEcologico(ListaPossiveis,Transporte,_);
                                                                                transporteMaisRapido(LV,_,Transporte)),!.

recomendacaoVarias(Ecomendas,Transporte/Distancia/Caminho) :- predsort(cmpEncomendaTempo,Ecomendas,Sorted),
                                                            ruaLista(Sorted,Destinos),
                                                            fullyCaminho(Destinos,Distancia,Caminho),
                                                            findall(P,(ecomenda(E,_,P,_,_),member(E,Ecomendas)),LP), sumLista(LP,Peso),
                                                            findall(Veiculo/VelocidadePenalizada, 
                                                            (transporte(Veiculo,Max,_), Peso =< Max, velocidadeTransporte(Veiculo,Peso,VelocidadePenalizada)), LV),
                                                            toPrimeiro(Sorted,DistanciaPrimeiro,TempoPrimeiro),
                                                            veiculosPossiveis(LV,DistanciaPrimeiro,TempoPrimeiro,ListaPossiveis),
                                                            length(ListaPossiveis,LengthLista),
                                                            (LengthLista =\= 0 -> veiculoMaisEcologico(ListaPossiveis,Transporte,_);
                                                            transporteMaisRapido(LV,_,Transporte)),!.                                                             

toPrimeiro([H|_],Distancia,Tempo) :- ecomenda(H,Destino,_,_,Tempo),sede(Rua),resolve_iter(Destino,Rua,Caminho),distancia(Caminho,Distancia).

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