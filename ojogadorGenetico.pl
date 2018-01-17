%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ojogadorGenetico
% * Funcoes de avaliacao e bot

func_avaliacao(1,chucknorris).
func_avaliacao(2,chucknorris).
func_avaliacao(3,chucknorris).
func_avaliacao(4,chucknorris).
func_avaliacao(5,chucknorris).
func_avaliacao(6,chucknorris).
func_avaliacao(7,chucknorris).
func_avaliacao(8,chucknorris).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numeroDeJogador(+Posicao,-Num).
% * Dada a posicao (norte/sul) em que estou a jogar, devolve em Num o meu numero (1,2,...) de jogador
%   serah util para escolher o cromossoma a utilizar...
numeroDeJogador(Posicao,Num) :-
	jogador(Posicao,Num,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% avalicoes([A1,A2,A3,...]).
% * Lista com as funcoes de avaliacao disponiveis
% * Todas as funcoes de avaliacao levam 3 argumentos: +Jogador, +Tabuleiro e -Valor
% * Todas as funcoes de avaliacao tem como caracteristica devolver valores
%   negativos caso estejam a avaliar para o adversario.
% * Todas as funcoes de avaliacao avaliam a mesma componente para ambos os jogadores
%   e a ordem de colocacao na lista eh Sul,Norte.
avaliacoes([vaziosSul, vaziosNorte, comMaisDe12Sul, comMaisDe12Norte,
	    chegaAoOutroSul, chegaAoOutroNorte, scoreSul, scoreNorte]).

% cromossomas "artificiais" para testes. Retirar!!!
%  1 - baseado no bot chucknorris
%  2 - baseado no bot samurai
cromossoma(1,[-8, 0, 2, 0, 6, 9, 9,-7]).
cromossoma(2,[-4, 0, 1, 0, 4,-6, 8,-8]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% saca_info(+Tabuleiro,-ListaSul,-ListaNorte,-ScoreSul,-ScoreNorte).
% * Dado um Tabuleiro devolve a lista de pecas do sul e do norte e os respectivos scores.
saca_info(tabuleiro(_,_,_,SN,SS,S1-S2-S3-S4-S5-S6+N1-N2-N3-N4-N5-N6), [S1,S2,S3,S4,S5,S6], [N1,N2,N3,N4,N5,N6],SS,SN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vaziosSul / vaziosNorte
% * Conta o numero de casas vazias
contaZeros([], Ac, Ac).
contaZeros([0|R], Ac, Zeros) :- !,
	NAc is Ac+1,
	contaZeros(R, NAc, Zeros).
contaZeros([_|R], Ac, Zeros) :-
	contaZeros(R, Ac, Zeros).

vaziosSul(_,Tabuleiro, ZerosSul) :-
	saca_info(Tabuleiro, LS, _,_,_),
	contaZeros(LS,0,ZerosSul).

vaziosNorte(_,Tabuleiro, ZerosNorte) :-
	saca_info(Tabuleiro, _, LN, _, _),
	contaZeros(LN,0,ZerosNorte).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comMaisDe12Sul / comMaisDe12Norte
% * Conta o numero de casas com mais de 12 pecas
contaMaisDe12([], Ac, Ac).
contaMaisDe12([Val|Resto], Ac, Numero) :-
	Val > 12, !,
	NAc is Ac + 1,
	contaMaisDe12(Resto,NAc,Numero).
contaMaisDe12([_|Resto], Ac, Numero) :-
	contaMaisDe12(Resto, Ac, Numero).

comMaisDe12Sul(_,Tabuleiro, NumSul) :-
	saca_info(Tabuleiro, LS, _, _, _),
	contaMaisDe12(LS,0, NumSul).

comMaisDe12Norte(_,Tabuleiro, NumNorte) :-
	saca_info(Tabuleiro, _, LN, _, _),
	contaMaisDe12(LN,0, NumNorte).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chegaAoOutroSul / chegaAoOutroNorte
% * Conta o numero de casas que consegue acabar a
%   jogada no tabuleiro do adversario
chegaAoOutroLado([],_,Ac,Ac).
chegaAoOutroLado([Val|Resto],Passo,Ac,Res) :-
	RVal is Val mod 11,
	RVal >= Passo,!,
	NAc is Ac + 1,
	NPasso is Passo - 1,
	chegaAoOutroLado(Resto,NPasso,NAc,Res).
chegaAoOutroLado([_|Resto],Passo,Ac,Res) :-
	NPasso is Passo - 1,
	chegaAoOutroLado(Resto,NPasso,Ac,Res).

chegaAoOutroSul(_,Tabuleiro,NumSul) :-
	saca_info(Tabuleiro, LS, _, _, _),
	chegaAoOutroLado(LS,6,0,NumSul).

chegaAoOutroNorte(_,Tabuleiro,NumNorte) :-
	saca_info(Tabuleiro, _, LN, _, _),
	chegaAoOutroLado(LN,6,0,NumNorte).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scoreSul / scoreNorte
% * Devolve o valor score do sul/norte
scoreSul(_,Tabuleiro,Valor) :-
	saca_info(Tabuleiro,_,_,Valor,_).

scoreNorte(_,Tabuleiro,Valor) :-
	saca_info(Tabuleiro,_,_,_,Valor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% valorFinal - dah valor aos estados finais,
%  1000 se vitoria minha
% -1000 se vitoria do outro
%     0 se empate
valorFinal(_,empate,0) :- !.
valorFinal(Nome,Nome,1000) :- !.
valorFinal(_,_,-1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% avaliaEstado(+Estado,+FuncsAvaliacao,+Cromossoma,-Valor).
% * Dado um estado, e as listas de funcoes de avaliacao e cromossoma
%   devolve o valor associado ao estado atravez da soma dos valores
%   ponderados de cada funcao de avaliacao
avaliaEstado(Jogador,Estado,FuncsAvaliacao,Cromossoma,Valor) :-
	avaliaEstado(Jogador,Estado,FuncsAvaliacao,Cromossoma,0,Valor).

avaliaEstado(_,_,[],[],Ac,Ac).

avaliaEstado(sul,Estado,[FSul,FNorte|RestoFuncoes],[GeneEu,GeneOutro|RestoCromossoma],Ac,Valor) :-
	AvalSul =.. [FSul,sul,Estado,VAvalSul], AvalSul,
	AvalNorte =.. [FNorte,sul,Estado,VAvalNorte], AvalNorte,
	VSul is VAvalSul * GeneEu,
	VNorte is VAvalNorte * GeneOutro,
	NAc is Ac + VSul + VNorte,
	avaliaEstado(sul, Estado, RestoFuncoes, RestoCromossoma, NAc, Valor).

avaliaEstado(norte,Estado,[FSul,FNorte|RestoFuncoes],[GeneEu,GeneOutro|RestoCromossoma],Ac,Valor) :-
	AvalSul =.. [FSul,norte,Estado,VAvalSul], AvalSul,
	AvalNorte =.. [FNorte,norte,Estado,VAvalNorte], AvalNorte,
	VSul is VAvalSul * GeneOutro,
	VNorte is VAvalNorte * GeneEu,
	NAc is Ac + VSul + VNorte,
	avaliaEstado(norte, Estado, RestoFuncoes, RestoCromossoma, NAc, Valor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chucknorris(+Jogador,+Estado,-Valor)
% * Avalia um estado do jogo para o jogador corrente.
chucknorris(Jogador,Estado,Valor) :-
	final(Estado,Val),
	valorFinal(Jogador,Val,Valor).

chucknorris(Jogador, Estado, Valor) :-
	numeroDeJogador(Jogador,NumJogador),
	cromossoma(NumJogador,Cromossoma),
	avaliacoes(FuncsAvaliacao),
	avaliaEstado(Jogador,Estado,FuncsAvaliacao,Cromossoma,Valor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
