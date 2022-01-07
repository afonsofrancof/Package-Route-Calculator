
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

