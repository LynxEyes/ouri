%nth1(+Idx,+Lista,-Elem,-Resto).

nth1(1,[Elem|Cauda],Elem,Cauda) :- !.
nth1(Num,[E|Cauda],Elem,[E|Resto]) :-
	Num > 1,
	NNum is Num - 1,
	nth1(NNum,Cauda,Elem,Resto).