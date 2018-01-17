%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% superLiga(+NumPJogos, +Prof, +ListaBots, -ListaScores, -ListaVitorias)  
% * Executa uma liga de "NumPJogos" pares de jogos entre cada possivel par de bots
%   dados em "ListaBots", sendo a procura com profundidade dada por "Prof"
% * Devolve em ListaScores o numero de partidas ganhas por cada bot
% * Devolve em ListaVitorias o numero de campeonatos ganhos por cada bot
superLiga(NumPJogos,Prof,ListaBots,ListaScores,ListaVitorias) :-
	todos_pares(ListaBots,ListaPares),
	inicializaSuperLiga(ListaBots),
	faz_jogos(NumPJogos,Prof,ListaPares),
	faz_ListasResultados(ListaBots,ListaScores,ListaVitorias).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% todos_pares(+Lista,-ListaPares).
% * Obtem todos os possiveis pares de elementos da lista, excluindo repeticoes
todos_pares(Lista, ListaPares) :-
	findall((X,Y),get_two(Lista,X,Y),ListaPares).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_one(+Lista,-Elem).
% * Devolve um elemento da lista dada.
% * Devolve cada elemento da lista por backtracking.
get_one([X|_],X).
get_one([_|Resto],X) :-
	get_one(Resto,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_two(+Lista,-E1,-E2).
% * Devolve 1 par de elementos diferentes da lista dada.
% * Devolve todos os pares diferentes por backtracking
get_two([X|Resto], X, Y) :-
	get_one(Resto,Y).
get_two([_|Resto],X,Y) :-
	get_two(Resto,X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inicializaSuperLiga(+Lista)
% * Dada uma lista de bots, inicializa os scores acumulados
%   e vitorias de cada bot a 0
inicializaSuperLiga([]).
inicializaSuperLiga([X|R]) :-
	assert(botscore(X,0)),
	assert(botvitorias(X,0)),
	inicializaSuperLiga(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% faz_jogos(+NumPJogos, +Prof, +ListaParesBots).
% * Dada uma lista de pares de bots, faz um miniCamp por cada
%   par com "NumPJogos" pares de jogos de profundidade "Prof".
% * Como efeito lateral, altera o numero de vitorias e o score
%   de cada bot ao longo dos miniCamp'eonatos realizados.
faz_jogos(_,_,[]).
faz_jogos(NumPJogos,Prof,[(X,Y)|Resto]) :-
	writeln('########### NOVO CAMPEONATO #############'),
	miniCamp(NumPJogos,Prof,X,Y,ScoreX,ScoreY),
	actualizaScores(X,Y,ScoreX,ScoreY),
	actualizaVitorias(X,Y,ScoreX,ScoreY),
	faz_jogos(NumPJogos,Prof,Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% contaScores(+Bot1, +Bot2, +ScoreBot1, +ScoreBot2).
% * Dados 2 bots e respectivos scores, actualiza os scores
%   dos bots adicionando ao score actual, o score obtido agora.
actualizaScores(X,Y,SX,SY) :-
	retract(botscore(X,VX)),
	retract(botscore(Y,VY)),
	NSX is SX + VX,
	NSY is SY + VY,
	assert(botscore(X,NSX)),
	assert(botscore(Y,NSY)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% actualizaVitorias(+Bot1,+Bot2,+ScoreBot1,+ScoreBot2).
% * Dados 2 bots e seus scores, verifica qual dos 2 ganhou
%   o miniCamp e actualiza o numero de vitorias do vencedor.
actualizaVitorias(X,_,SX,SY) :-
	SX > SY, !,
	retract(botvitorias(X,VX)),
	NVX is VX + 1,
	assert(botvitorias(X,NVX)).
actualizaVitorias(_,Y,SX,SY) :-
	SY > SX, !,
	retract(botvitorias(Y,VY)),
	NVY is VY + 1,
	assert(botvitorias(Y,NVY)).
actualizaVitorias(_,_,_,_) :- !. %para os empates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% faz_ListasResultados(+ListaBots,-ListaScores,-ListaVitorias).
% * Dado "ListaBots" (lista de bots), cria a lista de scores e de vitorias
%   depois de realizado uma superliga.
faz_ListasResultados([],[],[]).
faz_ListasResultados([X|R],[(X,SX)|RS],[(X,VX)|RV]) :-
	retract(botscore(X,SX)),
	retract(botvitorias(X,VX)),
	faz_ListasResultados(R,RS,RV).
