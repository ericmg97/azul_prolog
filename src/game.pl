:- [utils, visual].
:- dynamic shift/3, print_winners/2, print_winners/2, print_status/1, max/2, print_winners/1, print_round/1, print_status/2, replace/4, del/3, take/3.

%iniciar la partida dependiendo de la cantidad de jugadores
play(2) :- create_bag(Bag), board(1, B1), board(2, B2), start_round(Bag, [B1, B2], 5, 0).
play(3) :- create_bag(Bag), board(1, B1), board(2, B2), board(3, B3), start_round(Bag, [B1, B2, B3], 7, 0).
play(4) :- create_bag(Bag), board(1, B1), board(2, B2), board(3, B3), board(4, B4), start_round(Bag, [B1, B2, B3, B4], 9, 0).
play(_) :- write("La cantidad de jugadores debe ser: 2, 3 o 4.").


%crear la bolsa de losas
create_bag(Bag) :- create_bag(100, Pieces), flatten(Pieces, Bag).
create_bag(20, Bag):- findall(1, between(1, 20, _), Bag).
create_bag(X, [Y,Bag]):- 
    N is X // 20,
    Z is X - 20,
    findall(N, between(1, 20, _), Y),
    create_bag(Z, Bag).

%crear el tablero de los jugadores (escalera, pared, basura, puntuacion)
board(Id, B) :- stair(S), wall(W), garbage(G), B = [S, W, G, 0, Id].

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

%crea las factorias y devuelve ademas el estado de la bolsa y del suelo
%Status = [Fact1, Fact2, ..., FactN, Bag, Floor]
create_round(0, Bag, [Bag,[-1]]) :- !.
create_round(N, Bag, [F|Status]) :- 
    X is N - 1,
    random_permutation(Bag, OBag),
    take(4, OBag, F),
    del(4, OBag, RBag),
    create_round(X, RBag, Status).

%ejecuta el juego ronda por ronda
start_round(Bag, Players, Fact, Round) :- 
    
    %crea las factorias extrayendo de la bolsa para empezar la ronda
    create_round(Fact, Bag, Status),
    
    take(Fact, Status, Factories),
    del(Fact, Status, [RBag, Floor]),

    %imprime el estado de la partida
    print_round(Round),
    print_status(Players, Factories),

    %ejecuta una ronda de movimientos de jugadores dejando el resultado en RPlayers
    play_round(Players, Factories, Floor, RPlayers),

    %actualiza el estado del juego
    refresh_status(RPlayers, RBag, [RoundPlayers, RoundBag]),

    NewRound is Round + 1,

    %espera por la interaccion del usuario para continuar
    get_single_char(_),

    %chequea si la partida termina, si esto ocurre se muestra el vencedor y se termina la ejecucion,
    %en otro caso, se continua la simulacion
    (game_over(RoundPlayers, RoundBag, Fact) -> (print_round("FINAL"), print_status(RoundPlayers), get_winner(RoundPlayers)); start_round(RoundBag, RoundPlayers, Fact, NewRound)).
    
%ejecuta la jugada de todos los jugadores hasta que el suelo y las factorias se queden vacias
play_round(Players, Factories, Floor, RPlayers) :- 
    make_all_play(Players, Factories, Floor, [NewPlayers, NewFactories, NewFloor]),
    
    (end_round(NewFactories, NewFloor) -> RPlayers = NewPlayers; play_round(NewPlayers, NewFactories, NewFloor, RPlayers)).

%ejecuta un movimiento por jugador
make_all_play(Players, Factories, Floor, RStatus) :- length(Players, L), make_all_play(Players, L, Factories, Floor, RStatus).

make_all_play(Players, 0, Factories, Floor, RStatus) :- RStatus = [Players, Factories, Floor], !.
make_all_play(Players, PL, Factories, Floor, RStatus) :- 
    length(Players, L),
    P is L - PL + 1,

    nth1(P, Players, Player),

    %el jugador player, realiza un movimiento y Neworna el estado de su tablero, de las factorias y del suelo
    player_move(Player, Factories, Floor, [NewPlayer, NewFactories, NewFloor]),

    %actualiza en la lista de jugadores, el tablero del jugador que jugó 
    replace(P, Players, NewPlayer, NewPlayers),

    I is PL - 1,
    make_all_play(NewPlayers, I, NewFactories, NewFloor, RStatus).

%actualiza el estado del tablero, incluyendo: 
%-escaleras, paredes y puntuaciones de los jugadores
%-orden de los jugadores para la siguiente ronda
%-fichas sobrantes de la ronda puestas en la bolsa
refresh_status(Players, Bag, RBoard) :- 
    length(Players, P),
    check_first(Players, P, Player),
    shift(Players, Player, OPlayers)
    %refresh_board(OPlayers, Bag, [Board|Bag])

    .

% retorna el jugador que escogio la ficha FIRST
check_first(Players, FPlayer) :- length(Players, P), check_first(Players, P, FPlayer).
check_first([P|_], 0, FPlayer) :- nth0(4, P, Id), FPlayer = Id, !.
check_first(Players, P, FPlayer):-
    length(Players, L),
    Actual is L - P,

    nth0(Actual, Players, Player),
    nth0(2, Player, Garbage),
    nth0(4, Player, Id),

    I is P - 1,

    (member(_:-1, Garbage) -> FPlayer = Id; check_first(Players, I, FPlayer)).


%rellena la bolsa con las fichas que sobraron de las escaleras de los jugadores y en sus basuras
refill_bag(Players, Bag, RBag) :- !.

%chequea si se acabaron las losas en las factorias y en el suelo, si es asi, termina la ronda
end_round([], []) :- !.
end_round(Factories, Floor) :- 
    Floor == [],
    forall(member(X, Factories), X == []).

%obtiene el mejor jugador y su puntuacion
get_winner(Players) :- 
    length(Players, L),
    get_winner(Players, L, 0, []).

get_winner(_, 0, Best, Winners) :- print_winners(Best, Winners), !.
get_winner(Players, Actual, Best, Winners) :-
    length(Players, L),
    NActual is L - Actual + 1,

    nth1(NActual, Players, Player),
    nth0(3, Player, Punctuation),
    max([Best, Punctuation], NewBest),
    
    nth0(4, Player, Id),

    (Punctuation == Best ->  append(Winners, [Id], NewWinners);
    (Punctuation == NewBest -> NewWinners = [Id]; NewWinners = Winners)),

    Act is Actual - 1,
    get_winner(Players, Act, NewBest, NewWinners).
 
%chequea si el juego se acabo
game_over(Players, Bag, Fact) :-
    length(Bag, BL),
    (game_over_bag(BL, Fact);
    game_over_players(Players)).

game_over_bag(BL, Fact) :- BL < Fact*4.
game_over_players([]) :- false, !.
game_over_players([P | Players]) :-
    nth0(1, P, Wall),
    (member([_:1, _:1, _:1, _:1, _:1], Wall) -> !; game_over_players(Players)).



%TESTER
%RetStatus = [[[[],[[2:1,3:1,4:1,5:1,6:0],[]],[],15],[[],[[2:1,3:1,4:1,5:1,6:0],[]],[],15]], [[],[2]], Bag, []],
