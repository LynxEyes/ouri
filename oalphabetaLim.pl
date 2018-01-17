%				Mundo Alfa-Beta


% O predicado qual alfabeta leva ja' em conta que os valores iniciais do alfa e do 
% beta sao respectivamente -1000 e 1000 e tambem que o predicado que devolve uma
% jogada a partir de um estado e' o jogada/3. Este predicado ignora o valor que
% resulta da melhor jogada mas apenas esta' interessado na melhor jogada e no
% tabuleiro resultante

qual_alpha_beta(Nome,Estado,Lim,MelhorJog,MelhorSuc,Avalia) :-
       alphabeta(Nome,Estado,Lim,-1000,1000,MelhorJog,MelhorSuc,_,jogada,Avalia).


% O predicado qual alfabeta leva ja' em conta que os valores iniciais do alfa e do 
% beta sao respectivamente -1000 e 1000 e tambem que o predicado que devolve uma
% jogada a partir de um estado e' o jogada/3. Este predicado devolve tambem o valor que
% resulta da melhor jogada mas apenas esta' interessado na melhor jogada e no
% tabuleiro resultante

qual_alpha_beta(Nome,Estado,Lim,MelhorJog,MelhorSuc,Valor,Avalia) :-
       alphabeta(Nome,Estado,Lim,-1000,1000,MelhorJog,MelhorSuc,Valor,jogada,Avalia).




% ----------------------------------------------------

% algoritmo minimax alfa-beta, adaptado do Ivan Bratko

% ----------------------------------------------------


% Este predicado eh generico pois passamos como argumento o nome do predicado que faz
% a lista de jogadas possiveis (estados resultantes). Ha outro argumento tambem que eh
% o nome do predicado que avalia cada estado do jogo.

% O nome eh do jogador que estah a pensar e nao eh alternado. O que eh alternado eh o max-min
% dai o nome nao mudar ao longo das chamadas recursivas.
% O nome vai sair porque nao eh necessario.
% Assumimos que se for necessario farah parte do estado.
% Sabendo quem joga a seguir e se o nivel eh max ou min sabe-se sempre quem estah a pensar.
% Eh menos um argumento.................

alphabeta(Nome,Pos,Lim,Alpha,Beta,BestJog,BestPos,BestVal,MovePred,StaticValPred):-
      Lim >= 1,
      % Calculo de todos os sucessores do no
      setof(Jog-Suc, G^(G=..[MovePred,Pos,Suc,Jog],G),NList),
       shuffle_list(NList,List),
      goooon(Nome,List,Lim,Alpha,Beta,BestJog,BestPos,BestVal,MovePred,StaticValPred).

goooon(_,[Jog-Pos],_,_,_,Jog,Pos,unknown,_,_) :- !.
goooon(Nome,[Jog-Pos|PosList],Lim,Alpha,Beta,BestJog,BestPos,BestVal,Move,Aval):-
      goon(Nome,[Jog-Pos|PosList],Lim,Alpha,Beta,Jog,Pos,BestJog,BestPos,BestVal,Move,Aval).


% Nao ha mais sucessores...ou nao houve

goon(_,[],_,Alpha,_,GoodJog,GoodPos,GoodJog,GoodPos,Alpha,_,_).

goon(Nome,[Jog-Pos|PosList],Lim,Alpha,Beta,OldJog,OldPos,BestJog,BestPos,BestVal,Move,Aval):-
      NLim is Lim - 1,
      mnmxab(Nome,min,Pos,NLim,Alpha,Beta,Val,Move,Aval),!,
      actualizaMax(Alpha,OldJog,OldPos,Jog,Pos,Val,GoodJog,GoodPos,GoodAlpha),
      goon(Nome,PosList,Lim,GoodAlpha,Beta,GoodJog,GoodPos,BestJog,BestPos,BestVal,Move,Aval).




% Minimax alphabeta para os no's interiores e folhas (diferentes da rai­z) em que basta
% saber o melhor (max ou min) do valor dos no's sucessores e nao qual o melhor sucessor e
% qual a melhor jogada.

mnmxab(Nome,_,Pos,0,_,_,Val,_,StaticValPred):-
      % atingimos o limite, o no vai ser avaliado
      V=..[StaticValPred,Nome,Pos,Val],
      V,
      !.

mnmxab(Nome,Type,Pos,Lim,Alpha,Beta,Val,Move,Aval):-
      % Calculo de todos os sucessores do no
      findall(Suc, (G=..[Move,Pos,Suc,_],G),[FPos|PosList]),
       shuffle_list([FPos|PosList],List),
      % Nao ha retrocesso neste ponto
      !,
      % Vamos examinar cada um deles
      boundedbest(Nome,Type,List,Lim,Alpha,Beta,Val,Move,Aval),
      !.

mnmxab(Nome,_,Pos,_,_,_,Val,_,StaticValPred):-
      % O no nao tem sucessores entao eh uma folha e vai ser avaliado
      V=..[StaticValPred,Nome,Pos,Val],
      V,
      !.


% Corte (alpha maior do que beta)
boundedbest(_,max,_,_,Alpha,Beta,Alpha,_,_) :-
      Alpha >= Beta,!.
boundedbest(_,min,_,_,Alpha,Beta,Beta,_,_) :-
      Alpha >= Beta,!.


boundedbest(Nome,Type,[Pos|PosList],Lim,Alpha,Beta,BestVal,Move,Aval):-
      trocamm(Type,Type1),
      NLim is Lim - 1,
      mnmxab(Nome,Type1,Pos,NLim,Alpha,Beta,Val,Move,Aval),!,
      actualiza(Type,Pos,Alpha,Beta,Val,Alpha1,Beta1),
      boundedbest(Nome,Type,PosList,Lim,Alpha1,Beta1,BestVal,Move,Aval).


% Nao ha mais sucessores...

boundedbest(_,max,[],_,Alpha,_,Alpha,_,_).

boundedbest(_,min,[],_,_,Beta,Beta,_,_).


% Actualiza o alpha e o beta caso aparecem melhores valores senao mantem-se

actualiza(max,_,Alpha,Beta,Val,Val,Beta) :-
      Val > Alpha,!.
actualiza(min,_,Alpha,Beta,Val,Alpha,Val) :-
      Val < Beta,!.
actualiza(max,_,Alpha,Beta,_,Alpha,Beta).
actualiza(min,_,Alpha,Beta,_,Alpha,Beta). % Este deve ser redundante, pondo _ no arg 1

% Actualiza o alpha e o melhor sucessor caso apareca um valor maior
% senao ambos se mantem
% Este predicado eh soh utilizado pela raiz

actualizaMax(Alpha,_,_,Jog,Pos,Val,Jog,Pos,Val) :-
      Alpha < Val,!.
actualizaMax(Alpha,OldJog,OldPos,_,_,_,OldJog,OldPos,Alpha).


% troca de nÃ­vel max para min e vice-versa

trocamm(max,min).
trocamm(min,max).


% O maior de dois numeros

max(X,Y,Y) :- Y > X,!.
max(X,_,X) :- !.


% O menor de dois numeros

min(X,Y,Y) :-  Y < X,!.
min(X,_,X) :- !.






% -------------------
% Baralha uma lista
% -------------------

shuffle_list(L,Ls) :-
  length(L,D),
  N is D+1,
  shuffle_list(L,N,Ls).
shuffle_list([],_,[]).
shuffle_list([X],_,[X]).
shuffle_list(L,N,[P|R]) :-
  random(1,N,X),
  nth1(X,L,P,La),
  Nn is N-1,
  shuffle_list(La,Nn,R).
