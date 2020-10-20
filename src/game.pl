:- [utils].
:- dynamic replace/4, del/3, take/3.

%iniciar la partida dependiendo de la cantidad de jugadores
play(2, Winner) :- create_bag(Bag), board(B1), board(B2), start_round(Bag, [B1, B2], 5, Winner).
play(3, Winner) :- create_bag(Bag), board(B1), board(B2), board(B3), start_round(Bag, [B1, B2, B3], 7, Winner).
play(4, Winner) :- create_bag(Bag), board(B1), board(B2), board(B3), board(B4), start_round(Bag, [B1, B2, B3, B4], 9, Winner).
play(_, _) :- write("La cantidad de jugadores debe ser: 2, 3 o 4.").


%crear la bolsa de losas
create_bag(Bag) :- create_bag(100, Pieces), flatten(Pieces, OBag), random_permutation(OBag, Bag).
create_bag(20, Bag):- findall(1, between(1, 20, _), Bag).
create_bag(X, [Y,Bag]):- 
    N is X // 20,
    Z is X - 20,
    findall(N, between(1, 20, _), Y),
    create_bag(Z, Bag).

%crear el tablero de los jugadores (escalera, pared, basura, puntuacion)
board(B) :- stair(S), wall(W), garbage(G), B = [S, W, G, 0].

%crear escalera de jugador
stair(X):- X = [[0],
	            [0, 0],
	            [0, 0, 0],
	            [0, 0, 0, 0],
	            [0, 0, 0, 0, 0]].

%crear pared del jugador
wall(X):- X =  [[1:0, 2:0, 3:0, 4:0, 5:0],
	            [5:0, 1:0, 2:0, 3:0, 4:0],
	            [4:0, 5:0, 1:0, 2:0, 3:0],
	            [3:0, 4:0, 5:0, 1:0, 2:0],
	            [2:0, 3:0, 4:0, 5:0, 1:0]].

%crear basurero del jugador
garbage(X):- X = [-1:0, -1:0, -2:0, -2:0, -2:0, -3:0, -3:0].

start_round(Bag, Players, Fact, Winner) :- 
    length(Bag, Pieces),
    (Pieces =< Fact*4 -> get_winner(Players, Winner),!),
    
    create_factories(Fact, Bag, Status),
    take(Fact, Status, Factories),
    nth0(Fact, Status, RBag),

    print_status(Players, Factories),

    play_round(Players, Factories, RPlayers),

    print_status(RPlayers),

    (game_over(RPlayers) -> get_winner(Players, Winner),!),

    start_round(RBag, RPlayers, Fact, Winner).


%crea las factorias y devuelve ademas el estado de la bolsa
create_factories(0, Bag, [Bag]) :- !.
create_factories(N, Bag, [F|Status]) :- 
    X is N - 1,
    take(4, Bag, F),
    del(4, Bag, RBag),
    create_factories(X, RBag, Status).

