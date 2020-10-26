% toma los N primeros elementos de la lista
take(0, _, []).
take(_, [], []).
take(N, [X|Y], [X|W]):-
	M is N-1,
	take(M, Y, W).

% elimina los N primeros elementos de la lista
del(N, X, Y) :- length(Z, N), append(Z, Y, X).


%reemplaza el elemento I esimo de la lista L por E
replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).

% corre los elementos de la lista L1, N veces hacia la izquierda
shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N

% retorna el indice del elemento Element de una lista
indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1),
    !,
    Index is Index1+1.

% para el trabajo con matrices
matrix(Matrix, I, J, Value) :-
    nth0(I, Matrix, Row),
    nth0(J, Row, Value).

% retorna el elemento que mas se repite en la lista L
max_repeated(L, M) :-
    setof(I-E, C^(aggregate(count, member(E, L), C), I is -C), [_-M|_]).