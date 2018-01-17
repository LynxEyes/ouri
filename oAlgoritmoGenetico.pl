%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parteLista(+Lista,+Posicao,-Parte1,-Parte2).
% * Parte uma lista em duas. a divisao eh feita na posicao dada.
parteLista([Elem|Resto], 1, [Elem], Resto).
parteLista([Elem|Resto], N, [Elem|Frente], Traz) :-
	NN is N - 1,
	parteLista(Resto, NN, Frente, Traz).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cruzaListas(+Lista1,+Lista2,-ListaMisturada1,-ListaMisturada2).
% PRE-COND: ambas as listas tem o mesmo tamanho.
% * Faz o cruzamento entre duas listas num ponto escolhido aleatoriamente.
cruzaListas(Lista1, Lista2, NovaLista1, NovaLista2) :-
	length(Lista1, Len),
	random(1, Len, Div),
	parteLista(Lista1, Div, Lista1Parte1, Lista1Parte2),
	parteLista(Lista2, Div, Lista2Parte1, Lista2Parte2),
	append(Lista1Parte1,Lista2Parte2,NovaLista1),
	append(Lista2Parte1,Lista1Parte2,NovaLista2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% geraCromossoma(-Cromossoma)
% * Gera um novo cromossoma aleatorio
geraCromossoma([W1,W2,W3,W4,W5,W6,W7,W8]) :-
	random(0,10,W1),
	random(0,10,W2),
	random(0,10,W3),
	random(0,10,W4),
	random(0,10,W5),
	random(0,10,W6),
	random(0,10,W7),
	random(0,10,W8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% geraNCromossomas(+N,-ListaCromossomas).
% * Gera uma lista de cromossomas aleatorios
geraNCromossomas(0,[]).
geraNCromossomas(N,[C|Resto]) :-
	NN is N - 1,
	geraCromossoma(C),
	geraNCromossomas(NN,Resto).