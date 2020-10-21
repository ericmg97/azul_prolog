% toma los N primeros elementos de la lista
take(0, _, []).
take(_, [], []).
take(N, [X|Y], [X|W]):-
	M is N-1,
	take(M, Y, W).

% elimina los N primeros elementos de la lista
del(N, X, Y) :- length(Z, N), append(Z, Y, X).

%reemplaza el elemento I de la lista L por E
replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).
  
% retorna el maximo de una lista
max(L, M) :- max(L, -1, M).
max([], M, M).
max([X|Y], M, R):- (X > M -> K = X ; K = M), max(Y, K, R).
