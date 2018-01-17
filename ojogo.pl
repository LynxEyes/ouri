:- set_prolog_flag(syntax_errors,quiet).
:- dynamic jogador/3.
:- dynamic quero_parar/0.

initial :-
 consult(ojogadorGenetico),
 consult(oalphabetaLim),
 consult(oOuril),
 consult(adaptability_module),
 consult(championships).

:- initial.



% ------------------------------------------------------
% Executa um minicampeonato de N jogos entre Jog1 e Jog2
% Define o Limite de profundidade e
% Devolve o numero de Vitorias de cada um dos jogadores

miniCamp(N,Lim,Jog1,Jog2,VJog1,VJog2) :-
	n_paresRegista(N,Lim,Jog1,Jog2,L),
	numVitorias(Jog1,Jog2,L,VJog1,VJog2).

% -------------------------------------------------------------
% realiza N par de jogos entre dois jogadores e regista as 
% vitorias de cada um dos jogadores

n_paresRegista(0,_,_,_,[]).
n_paresRegista(N,Lim,Jog1,Jog2,[V1,V2|Res]) :-
       N > 0,
       par_jogosVits(Lim,Jog1,Jog2,[V1,V2]),
       NN is N - 1,
       n_paresRegista(NN,Lim,Jog1,Jog2,Res).

% Conta o numero de vitorias de cada um dos jogadores
% Dada a lista de vitorias de cada um dels

numVitorias(_,_,[],0,0).
numVitorias(J1,J2,[empate|R],V1,V2) :-
	numVitorias(J1,J2,R,V1,V2).
numVitorias(J1,J2,[J1|R],NV1,V2) :-
	numVitorias(J1,J2,R,V1,V2),
	NV1 is V1 + 1.
numVitorias(J1,J2,[J2|R],V1,NV2) :-
	numVitorias(J1,J2,R,V1,V2),
	NV2 is V2 + 1.

% -------------------------------------------------------------
% realiza N par de jogos sentre dois jogadores e guarda os logs

n_pares(0,_,_,_).
n_pares(N,Lim,Jog1,Jog2) :-
       N > 0,
       par_jogos(Lim,Jog1,Jog2,_,_),
       NN is N - 1,
       n_pares(NN,Lim,Jog1,Jog2).

% ---------------------------------------------------------------------------
%
%       Dois Jogos alternados entre dois jogadores
%
% ---------------------------------------------------------------------------

% Realiza um par de jogos entre dois jogadores e
% guarda o vencedor ou empate (Score e Tempos medio),
% guardando toda a informacao sobre os dois jogos.

par_jogos(Lim,Jog1,Jog2,Res1,Res2) :-
       nl,nl,tabX(20),write('UM PAR DE JOGOS '),nl,nl,
       jogo(Lim,Jog1,Jog2,Res1,Log1),
       assert(log(Lim,Jog1,Jog2,Res1,Log1)),
       jogo(Lim,Jog2,Jog1,Res2,Log2),
       assert(log(Lim,Jog2,Jog1,Res2,Log2)).


par_jogosVits(Lim,Jog1,Jog2,[Venc1,Venc2]) :-
       nl,nl,tabX(20),write('UM PAR DE JOGOS '),nl,nl,
       jogoEsconde(Lim,Jog1,Jog2,Venc1-_,_),
       jogoEsconde(Lim,Jog2,Jog1,Venc2-_,_).

% ---------------------------------------------------------------------------
%
%       Um Jogo entre dois jogadores
%
% ---------------------------------------------------------------------------

% Predicado principal para a realizacao de um jogo entre dois jogadores
% podem jogar programas e humanos

jogo(Lim,Jog1,Jog2,IdVencedor-Score) :-
      setup(Jog1,Jog2),
      mostra_jogadores(Jog1,Jog2),
      um_jogo(Lim,Vencedor-Score),
      det_id_vencedor(Jog1,Jog2,Vencedor,IdVencedor),
      mostra_final(Jog1,Jog2,Vencedor,IdVencedor,Score).


% Jogo so entre programas
% Mostramos a evolucao do jogo
% e ve-se passo a passo com ou sem pausa

jogo(Lim,Jog1,Jog2,IdVencedor-Score,LogTempos) :-
      setup_progs(Jog1,Jog2),
      mostra_jogadores(Jog1,Jog2),
      um_jogo(Lim,Vencedor-Score,LogTempos),
      det_id_vencedor(Jog1,Jog2,Vencedor,IdVencedor),
      mostra_final(Jog1,Jog2,Vencedor,IdVencedor,Score,LogTempos).

% Jogo so entre programas
% Nao ha trace da evolucao do jogo
% Estamos apenas interessados no resultado final

jogoEsconde(Lim,Jog1,Jog2,IdVencedor-Score,LogTempos) :-
      setup_progs(Jog1,Jog2),
      mostra_jogadores(Jog1,Jog2),
      um_jogoEsconde(Lim,Vencedor-Score,LogTempos),
      det_id_vencedor(Jog1,Jog2,Vencedor,IdVencedor),
      mostra_final(Jog1,Jog2,Vencedor,IdVencedor,Score,LogTempos).

% ------------------------------------------------------------------------
%
%       SETUP de um jogo entre Nome1 e Nome2
%
% ------------------------------------------------------------------------

% setup para jogos que possam envolver dois humanos, um humano contra programa
% ou mesmo dois programas.

setup(Jog1,Jog2) :-
       primeiro_jogador(Prim),
       adversario(Prim,Seg),
       retractall(jogador(Prim,_,_)),
       retractall(jogador(Seg,_,_)),
       ( (func_avaliacao(Jog1,Eval1),
          assert(jogador(Prim,Jog1,Eval1))) ; assert(jogador(Prim,Jog1,humano)) ),
       ( (func_avaliacao(Jog2,Eval2),
          assert(jogador(Seg,Jog2,Eval2))) ; assert(jogador(Seg,Jog2,humano)) ),!.


% Setup para jogos entre dois programas

setup_progs(Jog1,Jog2) :-
       primeiro_jogador(Prim),
       adversario(Prim,Seg),
       retractall(jogador(Prim,_,_)),
       retractall(jogador(Seg,_,_)),
       func_avaliacao(Jog1,Eval1),
       assert(jogador(Prim,Jog1,Eval1)),
       func_avaliacao(Jog2,Eval2),
       assert(jogador(Seg,Jog2,Eval2)).

% ---------------------------------------------------------------------------------
%       Predicado que controla o jogo
%       entre dois jogadores que utilizam o  alphabeta com a mesma profundidade Lim
%       A permissao eh um booleano para mostrar os nos que sao avaliados e quais
%       os valores (para teste e debugging).
% ---------------------------------------------------------------------------------


% Os humanos tambem podem jogar

um_jogo(Lim,Vencedor-Score) :-
	estado_inicial(Estado),
	mostra_tab(Estado),
	primeiro_jogador(Prim),
	ei_joga(Prim,Estado,Lim,Vencedor,Score).

% So jogam programas
% Devolve o Log (lista de jogada-tempoDaJogada)
% + o tempo medio das jogadas de cada um dos jogadores

um_jogo(Lim,Vencedor-Score,Log-TempMedios1-TempMedios2) :-
	estado_inicial(Estado),
	mostra_tab(Estado),
	pausa,
	primeiro_jogador(Prim),
	ei_joga(Prim,Estado,Lim,Vencedor,Score,Log),
	tempos_medios(Log,TempMedios1,TempMedios2).


% Nao mostres os estados do tabuleiro jogada apos jogada
% com ou sem interrupcao

% So jogam programas
% Devolve o Log (lista de jogada-tempoDaJogada)
% + o tempo medio das jogadas de cada um dos jogadores

um_jogoEsconde(Lim,Vencedor-Score,Log-TempMedios1-TempMedios2) :-
	estado_inicial(Estado),
	primeiro_jogador(Prim),
	ei_jogaEsconde(Prim,Estado,Lim,Vencedor,Score,Log),
	tempos_medios(Log,TempMedios1,TempMedios2).


% Dados os IDs dos dois jogadores e a ordem (norte ou sul), sabendo se foi o norte ou o sul
% o vencedor determina o ID do vencedor.

det_id_vencedor(_,_,empate,empate) :-
	!.
det_id_vencedor(Jog1,_,Prim,Jog1) :-
	primeiro_jogador(Prim),!.
det_id_vencedor(_,Jog2,_,Jog2).


% ----------------------------------------------------------------------
% predicado que manda executar uma jogada a cada jogador, alternadamente
% ateh que o jogo acabe. Soh Podem jogar programas.
% ----------------------------------------------------------------------

% Fim do jogo, determina-se o vencedor e o score e uma pequena mensagem

ei_joga(_,Estado,_,Vencedor,Score,[]) :-
	final(Estado,Vencedor),
	score(Estado,Score),
	!.

% Ainda nao acabou o joguinho, eh um programa a jogar

ei_joga(Nome,Estado,Lim,Vencedor,Score,[MelhorJogada-Time|RestoJogadas]) :-
	jogador(Nome,_,Avaliacao),
	statistics(runtime,[_,_]),
	qual_alpha_beta(Nome,Estado,Lim,MelhorJogada,MelhorSuc,Avaliacao),
	statistics(runtime,[_,Time]),
%	info_jogador_maquina(Nome,MelhorJogada,Time),
	mostraJogada(MelhorJogada),
 	mostra_tab(MelhorSuc),nl,
	pausa,
	adversario(Nome,Outro),
	ei_joga(Outro,MelhorSuc,Lim,Vencedor,Score,RestoJogadas).



% ----------------------------------------------------------------------
% predicado que manda executar uma jogada a cada jogador, alternadamente
% ateh que o jogo acabe. Soh Podem jogar programas.
% Nao mostra o jogo passo a passo mas apenas o resultado
% ----------------------------------------------------------------------

% Fim do jogo, determina-se o vencedor e o score e uma pequena mensagem

ei_jogaEsconde(_,Estado,_,Vencedor,Score,[]) :-
	final(Estado,Vencedor),
	score(Estado,Score),
	!.

% Ainda nao acabou o joguinho, eh um programa a jogar

ei_jogaEsconde(Nome,Estado,Lim,Vencedor,Score,[MelhorJogada-Time|RestoJogadas]) :-
%	getJogada(Estado,Jog),write(Jog),write(' '),
	jogador(Nome,_,Avaliacao),
	statistics(runtime,[_,_]),
	once(qual_alpha_beta(Nome,Estado,Lim,MelhorJogada,MelhorSuc,Avaliacao)),
	statistics(runtime,[_,Time]),
%	mostraJogada(MelhorJogada),
% 	mostra_tab(MelhorSuc),nl,
	adversario(Nome,Outro),
	ei_jogaEsconde(Outro,MelhorSuc,Lim,Vencedor,Score,RestoJogadas).


% ----------------------------------------------------------------------
% predicado que manda executar uma jogada a cada jogador, alternadamente
% ateh que o jogo acabe. Podem jogar programas e humanos.
% ----------------------------------------------------------------------

% Fim do jogo, determina-se o vencedor e o score e uma pequena mensagem

ei_joga(_,Estado,_,Vencedor,Score) :-
      final(Estado,Vencedor),
      score(Estado,Score),
       !.

% Ainda nao acabou o joguinho, eh um humano a jogar

ei_joga(Nome,Estado,Lim,Vencedor,Score) :-
      jogador(Nome,_,humano),
      pede_jogada(Estado,Jogada,NovoEstado),
%	info_jogador((Nome,MelhorJogada),
      mostraJogada(Jogada),nl,
      mostra_tab(NovoEstado),
      adversario(Nome,Outro),
      ei_joga(Outro,NovoEstado,Lim,Vencedor,Score),!.


% Ainda nao acabou o joguinho, eh um programa a jogar

ei_joga(Nome,Estado,Lim,Vencedor,Score) :-
       jogador(Nome,_,Avaliacao),
       qual_alpha_beta(Nome,Estado,Lim,MelhorJogada,MelhorSuc,Avaliacao),
%	info_jogador_maquina(Nome,MelhorJogada,Time),   
	 mostraJogada(MelhorJogada),nl,
       mostra_tab(MelhorSuc),nl,
       adversario(Nome,Outro),
	( (jogador(Outro,_,humano),!) ; pausa ),
       ei_joga(Outro,MelhorSuc,Lim,Vencedor,Score).


% ------------------------------------
% Leitura de jogadas do teclado
% ------------------------------------

% Pede uma jogada do teclado ateh que seja jogada valida

pede_jogada(tabuleiro(JQ,J,C,N,S,Tab),Jogada,NovoTab) :-
      repeat,
      pede_jogada(Jogada),
      jogada(tabuleiro(JQ,J,C,N,S,Tab),NovoTab,Jogada).

pede_jogada(J) :-
	write('Jogada: '),
	read(J).
	
%valida(Tab,NovoT,J) :-


% -------------------------------
%  Informacao sobre os jogadores
% -------------------------------

% Mostra o jogador que joga

%mostra_quem_joga(Nome) :-
%      jogador(Nome,Quem,_),
%      nl,nl,tabX(20),write(Quem),write(': '),write('JOGADOR '),write(Nome),nl,nl.



% Apresenta os jogadores

mostra_jogadores(Jog1,Jog2) :-
       primeiro_jogador(Prim),
       adversario(Prim,Seg),
	tabX(25),
       write(Prim),write(' = '),write(Jog1),write('  CONTRA  '),
       write(Seg),write(' = '),write(Jog2),nl,nl.


% Mostra o resumo do jogo: jogadores, vencedor e numero de jogadas do jogo.

mostra_final(Jog1,Jog2,_,IDVencedor,Score) :-
	write('Fim do jogo entre '),
	write(Jog1),write(' ('),
	primeiro_jogador(P),
	write(P),
	write(') e '),
	write(Jog2),
	write(' ('),
	adversario(P,S),
	write(S),
	write(')'),nl,nl,
	write('...................................'),nl,
	write('Vencedor = jogador '),write(IDVencedor),
	nl,
	write('Score = '),write(Score),nl.

% Mostra o resumo do jogo: jogadores, vencedor e numero de jogadas do jogo.
% Quando soh ha jogadores programas

mostra_final(Jog1,Jog2,_,IDVencedor,Score,_-TM1-TM2) :-
	write('Fim do jogo entre '),
	write(Jog1),write(' ('),
	primeiro_jogador(P),
	write(P),
	write(') e '),
	write(Jog2),
	write(' ('),
	adversario(P,S),
	write(S),
	write(')'),nl,nl,
	write('...................................'),nl,
	write('Vencedor = jogador '),write(IDVencedor),
	nl,
	write('Score = '),write(Score),nl,
	write('Tempo Medio jogador '),write(Jog1),write(' = '),write(TM1),nl,
	write('Tempo Medio jogador '),write(Jog2),write(' = '),write(TM2),nl.


% Dado o Log de um jogo e um jogador (brancas ou pretas) calculamos os tempos medios
% das suas jogadas

tempos_medios([],0,0) :-
	!.
tempos_medios(L,TempoMedioS,TempoMedioN) :-
       tempos_medios(L,0,0,TempoMedioS,0,0,TempoMedioN).
tempos_medios([],T1,N1,Ts,T2,N2,Tn) :-
       Ts is T1 / N1,
       Tn is T2 / N2.
tempos_medios([_-T],T1,N1,Ts,T2,N2,Tn) :-
       Ts is (T1 + T) / (N1 + 1),
       Tn is T2 / N2.
tempos_medios([_-Ta,_-Tb|R],T1,N1,Ts,T2,N2,Tn) :-
       NT1 is T1 + Ta,
       NT2 is T2 + Tb,
       NN1 is N1 + 1,
       NN2 is N2 + 1,
       tempos_medios(R,NT1,NN1,Ts,NT2,NN2,Tn).

% -----------------------------------------
%		As pausas
%

pausa :-
	quero_parar,
	write('Escreva qq (xxx.): '),
	repeat,
	read(_),!.
pausa.


% switch para pausa

com_pausa :-
	quero_parar,!.
com_pausa :-
	assert(quero_parar).


% switch para sem pausa

sem_pausa :-
	\+ quero_parar,!.
sem_pausa :-
	retractall(quero_parar).

tabX(0).
tabX(N) :-
	N > 0,
	write(' '),
	N1 is N-1,
	tabX(N1).