
:- ensure_loaded(conhecimento).

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
