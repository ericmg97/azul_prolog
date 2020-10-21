:- [utils, visual].
:- dynamic print_winners/2, print_winners/2, print_status/1, max/2, print_winners/1, print_round/1, print_status/2, replace/4, del/3, take/3.

%iniciar la partida dependiendo de la cantidad de jugadores
play(2) :- create_bag(Bag), board(B1), board(B2), start_round(Bag, [B1, B2], 5, 0).
play(3) :- create_bag(Bag), board(B1), board(B2), board(B3), start_round(Bag, [B1, B2, B3], 7, 0).
play(4) :- create_bag(Bag), board(B1), board(B2), board(B3), board(B4), start_round(Bag, [B1, B2, B3, B4], 9, 0).
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

%crea las factorias y devuelve ademas el estado de la bolsa y del suelo
%Status = [Fact1, Fact2, ..., FactN, Bag, Floor]
create_round(0, Bag, [Bag,[]]) :- !.
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
    del(Fact, Status, BFStatus),

    %imprime el estado de la partida
    print_round(Round),
    print_status(Players, Factories),
    
    %ejecuta una ronda dejando en RStaatus el resultado de la misma
    %RStatus = [[Player1, Player2, ... , PlayerN], Bag]
    play_round(Players, Factories, BFStatus, RStatus),
    
    nth0(0, RStatus, RPlayers),
    nth0(1, RStatus, RBag),

    Ro is Round + 1,

    %espera por la interaccion del usuario para continuar
    get_single_char(_),

    %chequea si la partida termina, si esto ocurre se muestra el vencedor y se termina la ejecucion,
    %en otro caso, se continua la simulacion
    (game_over(RPlayers, RBag, Fact) -> (print_round("FINAL"), print_status(RPlayers), get_winner(RPlayers)); start_round(RBag, RPlayers, Fact, Ro)).
    
%ejecuta la jugada de todos los jugadores hasta que el suelo y las factorias se queden vacias
play_round(Players, Factories, BFStatus, RStatus) :- 
    nth0(0, BFStatus, Bag),
    nth0(1, BFStatus, Floor),
    play_round(Players, Factories, Bag, Floor, RStatus).

play_round(Players, Factories, Bag, Floor, RStatus) :- 
    make_play(Players, Factories, Bag, Floor, RetStatus),

    %RetStatus = [[[[],[[2:1,3:1,4:1,5:1,6:0],[]],[],15],[[],[[2:1,3:1,4:1,5:1,6:0],[]],[],15]], [[],[2]], Bag, []],

    nth0(0, RetStatus, RetPlayers),
    nth0(1, RetStatus, RetFactories),
    nth0(2, RetStatus, RetBag),
    nth0(3, RetStatus, RetFloor),
    
    %CHEQUEAR FUNCIONAMIENTO CUANDO SE IMPLEMENTE MAKE_PLAY!!!!
    (end_round(RetFactories, RetFloor) -> RStatus = [RetPlayers,RetBag]; play_round(RetPlayers, RetFactories, RetBag, RetFloor, RStatus)).

make_play(Players, Factories, Bag, Floor, RStatus) :- !.

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
 
    (Punctuation == Best ->  append(Winners, [NActual], NewWinners);
    (Punctuation == NewBest -> NewWinners = [NActual]; NewWinners = Winners)),

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