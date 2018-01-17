:- use_module(library(lists)).


% ----------------------------------
% 			Teste 
% ----------------------------------

t :-
	estado_inicial(Tab),
	escolheUmBuraco(Tab,B,O,Tab1),
	posSeg(B,Bseg),
	once((
		getBuracos(Tab1,Burs),
		semeiaN(O,B,Bseg,Burs,NovoBurs,NovaPos),
		setBuracos(Tab1,NovoBurs,NovoTab),
	      	show(NovoTab),
	      	write('Posicao final = '),write(NovaPos),nl)),
	fail.

x(Tab) :-
	mostra_tab(Tab),
	jogada(Tab,Jogada,TabSeg),
	mostraJogada(Jogada),
	mostra_tab(TabSeg),
	read(_),
	fail.


% -----------------------

% 	Tabuleiro inicial

% -----------------------

estado_inicial(tabuleiro(X,0,0,0,0,4-4-4-4-4-4+4-4-4-4-4-4)) :-
	primeiro_jogador(X).

% --------------------
% A relacao adversario
% --------------------

adversario(norte,sul).
adversario(sul,norte).

% ------------------------------------
% O primeiro jogador e' o jogador sul
% ------------------------------------

primeiro_jogador(sul).


% -----------------------------------------------------------

%		Tabuleiros Finais

% -----------------------------------------------------------

final(tabuleiro(_,_,_,24,24,_),empate) :- !.
final(tabuleiro(_,_,_,SN,_,_), norte) :-
	SN > 24,!.
final(tabuleiro(_,_,_,_,SS,_), sul) :-
	SS > 24,!.
% Falta so' os casos de ciclo, pomos um limite de turnos
final(tabuleiro(_,_,N,S,S,_), empate) :-
	N > 100,!.
final(tabuleiro(_,_,N,SN,SS,_), norte) :-
	N > 100,
	SN > SS,!.
final(tabuleiro(_,_,N,_,_,_), sul) :-
	N > 100,!.

% -----------------------------

%	Faz uma Jogada

% -----------------------------

% O adversario nao tem pecas
% tem de se dar pecas
jogada(Tab,TabSeg,Jogada) :-
	adversarioSemPecas(Tab),
	jogaNormal(Tab,TabSg,Jogada),
	\+ adversarioSemPecas(TabSg),
	incJogada(TabSg,TabSeg).

% O adversario nao tem pecas
% nao ha' maneira de dar pecas
% quem joga fica recolhe todas as pecas
jogada(Tab,TabSeg,saca-todos) :-
	adversarioSemPecas(Tab),
	\+ ( jogaNormal(Tab,Tab2,_),
	     \+ adversarioSemPecas(Tab2) ),
	sacaTodas(Tab,Ganho),
	incScoreQuemJoga(Tab,Ganho,NovoTab),
	tabVazio(NovoTab,TabSg),
	incJogada(TabSg,TabSeg),!.

% O adversario tem pecas
% escolhe buraco e semeia
% Mas depois dessa jogada tem de se dar pecas
% ao adversario se ele ficar sem elas
jogada(Tab,TabSeg,Jogada) :-
	\+  adversarioSemPecas(Tab),
	jogaNormal(Tab,Tab1,Jogada1),
	outraVez(Tab1,TabSg,Jogada1,Jogada),
	incJogada(TabSg,TabSeg).

% -------------------------------------------------
% Continua a jogar se o adversario ficou sem pecas
% e se ainda nao esta' ganho
% -------------------------------------------------

% Nao, ja' ganhei
outraVez(Tab,Tab,Jogada,Jogada) :-
	final(Tab,_),!.
% Nao, ele ficou com pecas. Paro 
outraVez(Tab,Tab,Jogada,Jogada) :-
	\+ adversarioSemPecas(Tab),!.
% Jogo outra vez porque ficou sem pecas
% e consigo deixa-lo com pecas
outraVez(Tab,TabSeg,Jogada1,Jogada1+Jogada2) :-
	jogaNormal(Tab,TabSeg1,Jogada2),
	\+ adversarioSemPecas(TabSeg1),
	incJogada(TabSeg1,TabSeg2),
	incJogada(TabSeg2,TabSeg).
% Recolho tudo porque ele nao tem pecas
% e nao consigo jogar de modo a dar-lhe pecas
outraVez(Tab,TabSg,Jogada,Jogada+saca-todos) :-
	adversarioSemPecas(Tab),
	\+ ( jogaNormal(Tab,Tab2,_),
	     \+ adversarioSemPecas(Tab2) ),
	sacaTodas(Tab,Ganho),
	incScoreQuemJoga(Tab,Ganho,NovoTab),
	tabVazio(NovoTab,TabSeg1),
	incJogada(TabSeg1,TabSeg2),
	incJogada(TabSeg2,TabSg),!.

%% -------------------------------------------------
%% Joga normalmente, escolhendo um buraco e semeando
%% -------------------------------------------------

jogaNormal(Tab,TabSeg,Buraco) :-
	getJogador(Tab,Jog),
	getBuracos(Tab,Buracos),
	meuBuraco(Jog,Buraco),
	once((	buracoAManeira(Jog,Buracos,Buraco,Ouris,Buracos1),
		posSeg(Buraco,BuracoSeg),
		semeiaN(Ouris,Buraco,BuracoSeg,Buracos1,NovosBuracos,BuracoFinal),
		come(Jog,NovosBuracos,BuracoFinal,BuracosF,Ganho),
		incScoreQuemJoga(Tab,Ganho,NovoTab),
		setBuracos(NovoTab,BuracosF,TabSg),
		actGanhoContador(TabSg,Ganho,TabSeg) )).


% Verifica se o buraco e' "BOM" para "sacar" sementes.

buracoAManeira(_,Buracos,Buraco,Ouris,NovosBuracos) :-
	get(Buraco,Ouris,Buracos,NovosBuracos),
	Ouris >= 1.
% buracoAManeira(Jog,Buracos,Buraco,1,NovosBuracos) :-
%	todosMinimas(Jog,Buracos),
%	get(Buraco,1,Buracos,NovosBuracos).


% Devolve a posicao de um dos meus buracos

meuBuraco(norte,X) :-
	member(X,[7,8,9,10,11,12]).
meuBuraco(sul,X) :-
	member(X,[1,2,3,4,5,6]).


% Todas os buracos sao minimos

todosMinimas(Jog,Buracos) :-
	asPecas(Jog,Buracos,BuracosJog),
	menoresQue2(BuracosJog).


% -------------------------------------------------------

%		COMER

% -------------------------------------------------------

% Toca a comer as sementes se houverem para comer
% O Jog vai tentar comer sementes tendo ficado na posicao N
% V e' o numero de sementes comidas
% So' podera' comer se ficar no lado oposto ao seu e puder comer 
come(Jog,Tab,N,NovoTab,V) :-
	ladoOposto(Jog,N),	
	tocaAComer(Tab,N,0,NovoTab,V),!.
come(_,Tab,_,Tab,0).


% Toca a comer com o contador a V
% Se houver posicao anterior e N tiver 2 ou 3 sementes
% incrementa o ganho
% e continua a comer a partir da posicao anterior
tocaAComer(Tab,N,V,NovoTab,Vf) :-
	posAnt(N,NDec),
	get(N,X,Tab,Tab1),
	(X = 2 ; X = 3),
	V1 is V + X,
	tocaAComer(Tab1,NDec,V1,NovoTab,Vf),!.
% Nao ha' posicao anterior, acaba ja'
tocaAComer(Tab,N,V,NovoTab,Vf) :-
	get(N,X,Tab,NovoTab),
	(X = 2 ; X = 3),
	Vf is V + X,!.
tocaAComer(NovoTab,_,V,NovoTab,V).
	

% -------------------------------------------------------

%		semear

% -------------------------------------------------------

% -----------------------------------------
% Semeia N sementes no sentido anti-horario
% -----------------------------------------

semeiaN(N,Pos,Pos,Tab,NovoTab,NovaPos) :-
	posSeg(Pos,PosSeg),
	semeiaN(N,Pos,PosSeg,Tab,NovoTab,NovaPos).

semeiaN(1,_,NovaPos,Tab,NovoTab,NovaPos) :- 
	semeia1(NovaPos,Tab,NovoTab),!.

semeiaN(N,PosI,Pos,Tab,NovoTab,NovaPos) :-
	semeia1(Pos,Tab,Tab1),
	NN is N - 1,
	posSeg(Pos,PosI,Pos1),
	semeiaN(NN,PosI,Pos1,Tab1,NovoTab,NovaPos).

% ----------------
% Semeia um buraco
% ----------------

semeia1(1,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,NX1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12) :-
	NX1 is X1 + 1.
semeia1(2,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-NX2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12) :-
	NX2 is X2 + 1.
semeia1(3,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-NX3-X4-X5-X6+X7-X8-X9-X10-X11-X12) :-
	NX3 is X3 + 1.
semeia1(4,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-NX4-X5-X6+X7-X8-X9-X10-X11-X12) :-
	NX4 is X4 + 1.
semeia1(5,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-NX5-X6+X7-X8-X9-X10-X11-X12) :-
	NX5 is X5 + 1.
semeia1(6,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-NX6+X7-X8-X9-X10-X11-X12) :-
	NX6 is X6 + 1.
semeia1(7,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+NX7-X8-X9-X10-X11-X12) :-
	NX7 is X7 + 1.
semeia1(8,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-NX8-X9-X10-X11-X12) :-
	NX8 is X8 + 1.
semeia1(9,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-NX9-X10-X11-X12) :-
	NX9 is X9 + 1.
semeia1(10,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-X9-NX10-X11-X12) :-
	NX10 is X10 + 1.
semeia1(11,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-NX11-X12) :-
	NX11 is X11 + 1.
semeia1(12,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-NX12) :-
	NX12 is X12 + 1.



% -------------------------------------------------------

%		Ler o Tabuleiro

% -------------------------------------------------------

% ------------------------------------------
% Le o que ha no Tabuleiro numa dada posicao
% ------------------------------------------

le(1,X-_-_-_-_-_+_-_-_-_-_-_,X).
le(2,_-X-_-_-_-_+_-_-_-_-_-_,X).
le(3,_-_-X-_-_-_+_-_-_-_-_-_,X).
le(4,_-_-_-X-_-_+_-_-_-_-_-_,X).
le(5,_-_-_-_-X-_+_-_-_-_-_-_,X).
le(6,_-_-_-_-_-X+_-_-_-_-_-_,X).
le(7,_-_-_-_-_-_+X-_-_-_-_-_,X).
le(8,_-_-_-_-_-_+_-X-_-_-_-_,X).
le(9,_-_-_-_-_-_+_-_-X-_-_-_,X).
le(10,_-_-_-_-_-_+_-_-_-X-_-_,X).
le(11,_-_-_-_-_-_+_-_-_-_-X-_,X).
le(12,_-_-_-_-_-_+_-_-_-_-_-X,X).

% --------------------------------------------------------------
% vai buscar as sementes de um determinado buraco (esvaziando-o)
% --------------------------------------------------------------

get(1,Y,Y-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,0-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12).
get(2,Y,X1-Y-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-0-X3-X4-X5-X6+X7-X8-X9-X10-X11-X12).
get(3,Y,X1-X2-Y-X4-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-0-X4-X5-X6+X7-X8-X9-X10-X11-X12).
get(4,Y,X1-X2-X3-Y-X5-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-0-X5-X6+X7-X8-X9-X10-X11-X12).
get(5,Y,X1-X2-X3-X4-Y-X6+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-0-X6+X7-X8-X9-X10-X11-X12).
get(6,Y,X1-X2-X3-X4-X5-Y+X7-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-0+X7-X8-X9-X10-X11-X12).
get(7,Y,X1-X2-X3-X4-X5-X6+Y-X8-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+0-X8-X9-X10-X11-X12).
get(8,Y,X1-X2-X3-X4-X5-X6+X7-Y-X9-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-0-X9-X10-X11-X12).
get(9,Y,X1-X2-X3-X4-X5-X6+X7-X8-Y-X10-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-0-X10-X11-X12).
get(10,Y,X1-X2-X3-X4-X5-X6+X7-X8-X9-Y-X11-X12,X1-X2-X3-X4-X5-X6+X7-X8-X9-0-X11-X12).
get(11,Y,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-Y-X12,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-0-X12).
get(12,Y,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-Y,X1-X2-X3-X4-X5-X6+X7-X8-X9-X10-X11-0).



% ----------------------------------------------------------

%	Esvazia o tabuleiro (guradando o Total de sementes)
%	 (esvazia o seu lado porque o outros esta' supostamente vazio)

% ----------------------------------------------------------

sacaTodas(tabuleiro(norte,_,_,_,_,_-_-_-_-_-_+X6-X7-X8-X9-X10-X11),Total) :-
	Total is X6 + X7 + X8 + X9 + X10 + X11.
sacaTodas(tabuleiro(sul,_,_,_,_,X0-X1-X2-X3-X4-X5+_-_-_-_-_-_),Total) :-
	Total is X0 + X1 + X2 + X3 + X4 + X5.



% O adversario esta' sem peca

adversarioSemPecas(tabuleiro(norte,_,_,_,_,0-0-0-0-0-0+_-_-_-_-_-_)).
adversarioSemPecas(tabuleiro(sul,_,_,_,_,_-_-_-_-_-_+0-0-0-0-0-0)).


% As posicoes anteriores e seguintes

% Posicao seguinte
posSeg(12,1) :- !.
posSeg(X,Y) :-
	Y is X + 1.

posSeg(X,Proibido,Seg) :-
	posSeg(X,Seg),
	Seg =\= Proibido.
posSeg(X,Proibido,Seg) :-
	posSeg(X,Z),
	Z == Proibido,
	posSeg(Z,Seg).


% Posicao anterior
posAnt(X,Y) :-
	X > 7,
	Y is X - 1.

% Posicao anterior
posAnt(X,Y) :-
	X < 6,
	X > 0,
	Y is X - 1.

% -----------------------------------------------------------------------------------------
% Sera que um dado buraco esta no lado oposto de um dado jogador (no terreno do adversario)

ladoOposto(sul,M) :-
	M > 6.
ladoOposto(norte,N) :-
	N < 7.

% -----------------------------
% Vamos Buscar as pecas do Ouri

pecasNorteESul(P0-P1-P2-P3-P4-P5+P6-P7-P8-P9-P10-P11,[P11,P10,P9,P8,P7,P6],[P0,P1,P2,P3,P4,P5]).


% Vamos buscar as pecas do ouri de cada um dos jogadores

asPecas(norte,Pecas,PecasNorte) :-
	pecasNorteESul(Pecas,PecasNorte,_).

asPecas(sul,Pecas,PecasSul) :-
	pecasNorteESul(Pecas,_,PecasSul).


% --------------------------------------------

%	Toca a mostrar o tabuleiro

% --------------------------------------------


% mostra o tabuleiro

mostra_tab(tabuleiro(Jog,N,C,ScoreNorte,ScoreSul,Tab)) :-
	tabX(20),write('Score Norte = '),write(ScoreNorte),nl,nl,
	showTab(Tab), nl,
	tabX(20),write('Score Sul = '),write(ScoreSul),nl,nl,
	tabX(20),
	write(N),write(': '),write('Jogador = '),write(Jog),write('  contador = '),write(C),
	nl,nl.
		
% Desenha buracos

showTab(Pecas) :-
	pecasNorteESul(Pecas,PecasNorte,PecasSul),
	desenhaBuracos(PecasNorte),
	nl,
	desenhaBuracos(PecasSul).

% Desenha Buraquinhos

desenhaBuracos([]) :-
	nl.
desenhaBuracos([P|R]) :-
	desenhaBuraco(P),
	tabX(8),
	desenhaBuracos(R).

% Desenha um buraco

desenhaBuraco(X) :-
	write(X).

% Mostra a jogada

mostraJogada(X) :-
	nl,tabX(20),write('Jogada = '),write(X),nl,!.
mostraJogada(X+saca-todos) :-
	nl,tabX(20),write('Jogada = '),write(X),write(' + '), write('recolhe as sementes todas '),nl,!.
mostraJogada(X+Y) :-
	nl,tabX(20),write('Jogada = '),write(X),write(' + '), write(Y),nl,!.
mostraJogada(saca-todos) :-
	nl,tabX(20),write('Jogada = '), write('recolhe as sementes todas '),nl,!.
mostraJogada(X) :-
	nl,tabX(20),write('Jogada = '),write(X),nl,!.


% -----------------------

% 	Gets

% -----------------------

getJogador(tabuleiro(Jog,_,_,_,_,_),Jog).
getJogada(tabuleiro(_,Jogada,_,_,_,_),Jogada).
getContador(tabuleiro(_,_,Contador,_,_,_),Contador).
getScoreNorte(tabuleiro(_,_,_,ScoreNorte,_,_),ScoreNorte).
getScoreSul(tabuleiro(_,_,_,_,ScoreSul,_),ScoreSul).
getBuracos(tabuleiro(_,_,_,_,_,Buracos),Buracos).

% Sets

setJogador(tabuleiro(_,N,SN,SS,BS),Jog,tabuleiro(Jog,N,SN,SS,BS)).
setJogadas(tabuleiro(Jog,_,SN,SS,BS),N,tabuleiro(Jog,N,SN,SS,BS)).
setContador(tabuleiro(Jog,N,_,SN,SS,BS),X,tabuleiro(Jog,N,X,SN,SS,BS)).
setScoreNorte(tabuleiro(Jog,N,C,_,SS,BS),SN,tabuleiro(Jog,N,C,SN,SS,BS)).
setScoreSul(tabuleiro(Jog,N,C,SN,_,BS),SS,tabuleiro(Jog,N,C,SN,SS,BS)).
setBuracos(tabuleiro(Jog,N,C,SN,SS,_),Buracos,tabuleiro(Jog,N,C,SN,SS,Buracos)).


tabVazio(tabuleiro(Jog,N,C,SN,SS,_),tabuleiro(Jog,N,C,SN,SS,0-0-0-0-0-0+0-0-0-0-0-0)).

incScoreQuemJoga(Tab,0,Tab) :- !.
incScoreQuemJoga(Tab,Ganho,NovoTab) :-
	getJogador(Tab,norte),
	getScoreNorte(Tab,Score),
	Total is Score + Ganho,
	setScoreNorte(Tab,Total,NovoTab).
incScoreQuemJoga(Tab,Ganho,NovoTab) :-
	getJogador(Tab,sul),
	getScoreSul(Tab,Score),
	Total is Score + Ganho,
	setScoreSul(Tab,Total,NovoTab).


% Incrementa a jogada

incJogada(tabuleiro(Jog,Jogada,C,SN,Ss,Tab),tabuleiro(OutroJog,IncJogada,C,SN,Ss,Tab)) :-
	IncJogada is Jogada + 1,
	adversario(Jog,OutroJog).


actGanhoContador(tabuleiro(Jog,Jogada,C,SN,Ss,Tab),0,tabuleiro(Jog,Jogada,NC,SN,Ss,Tab)) :-
	NC is C + 1,!.
actGanhoContador(tabuleiro(Jog,Jogada,_,SN,SS,Tab),_,tabuleiro(Jog,Jogada,0,SN,SS,Tab)).
		

% ---------------------------------

% Predicados sobre listas

% ---------------------------------

% Todos os elementos da lista sao 0

todosIguaisA(_,[]).
todosIguaisA(X,[X|R]) :-
	todosIguaisA(X,R).

% Todos os elementos da lista ou sao 1 ou 0

menoresQue2([]).
menoresQue2([1|R]) :-
	menoresQue2(R).
menoresQue2([0|R]) :-
	menoresQue2(R).


% ------------------------------------------
%       O score eh soh o numero de jogadas
% ------------------------------------------

score(Tab,N) :-
	getScoreNorte(Tab,SN),
	getScoreSul(Tab,SS),
	N is abs(SN - SS).

% -----------------------
% Mostra todas as jogadas
% -----------------------

jogadasTodas(Estado) :-
	mostra_estado(Estado),
	jogada(Estado,NovoEstado,Jg),
	once( (nl,nl,mostra_jogada(Jg),nl,
	       mostra_tab(NovoEstado)) ),
	fail.
jogadasTodas(_).


