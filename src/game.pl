:- [utils, visual].
:- dynamic matrix/4, indexOf/3, shift/3, print_winners/2, print_winners/2, print_status/1, max/2, print_winners/1, print_round/1, print_status/2, replace/4, del/3, take/3.

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
    refresh_status(RPlayers, RBag, [NewPlayers, NewBag]),

    NewRound is Round + 1,

    %espera por la interaccion del usuario para continuar
    get_single_char(_),

    %chequea si la partida termina, si esto ocurre se muestra el vencedor y se termina la ejecucion,
    %en otro caso, se continua la simulacion
    (game_over(NewPlayers, NewBag, Fact) -> (print_round("FINAL"), print_status(NewPlayers), get_winner(NewPlayers)); start_round(NewBag, NewPlayers, Fact, NewRound)).
    
%ejecuta la jugada de todos los jugadores hasta que el suelo y las factorias se queden vacias
play_round(Players, Factories, Floor, RPlayers) :- 
    make_all_play(Players, Factories, Floor, [NewPlayers, NewFactories, NewFloor]),
    
    (end_round(NewFactories, NewFloor) -> RPlayers = NewPlayers; play_round(NewPlayers, NewFactories, NewFloor, RPlayers)).

%ejecuta un movimiento por jugador
make_all_play(Players, Factories, Floor, Status) :- length(Players, L), make_all_play(Players, L, Factories, Floor, Status).

make_all_play(Players, 0, Factories, Floor, Status) :- Status = [Players, Factories, Floor], !.
make_all_play(Players, PL, Factories, Floor, Status) :- 
    length(Players, L),
    P is L - PL + 1,

    nth1(P, Players, Player),

    %el jugador player, realiza un movimiento y retorna el estado de su tablero, de las factorias y del suelo
    player_move(Player, Factories, Floor, [NewPlayer, NewFactories, NewFloor]),

    %actualiza en la lista de jugadores, el tablero del jugador que jugó 
    replace(P, Players, NewPlayer, NewPlayers),

    I is PL - 1,
    make_all_play(NewPlayers, I, NewFactories, NewFloor, Status).

%actualiza el estado del tablero, incluyendo: 
%-escaleras, paredes y puntuaciones de los jugadores
%-orden de los jugadores para la siguiente ronda
%-fichas sobrantes de la ronda puestas en la bolsa
refresh_status(Players, Bag, RBoard) :- 
    length(Players, P),
    check_first(Players, P, Player),
    shift(Players, Player, OrderedPlayers),

    refresh_boards(OrderedPlayers, [NewPlayers, Trash]),
    
    %rellena la bolsa con las fichas que sobraron de las escaleras de los jugadores y en sus basuras
    append(Bag, Trash, NewBag),

    RBoard = [NewPlayers, NewBag].

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

%actualiza el estado de los tableros de los jugadores luego de una ronda
refresh_boards(Players, Status) :- length(Players, P), refresh_boards(Players, P, [], Status).
refresh_boards(Players, 0, Trash, Status) :- Status = [Players, Trash].
refresh_boards(Players, P, Trash, Status) :- 
    nth1(P, Players, Player),

    refresh_player_board(Player, [NewPlayer, NewTrash]),
    
    replace(P, Players, NewPlayer, NewPlayers),
    
    append(Trash, NewTrash, FinalTrash),

    I is P - 1,
    refresh_boards(NewPlayers, I, FinalTrash, Status).

%actualiza el estado del tablero del jugador al finalizar la ronda y retorna ademas las losas descartadas
refresh_player_board([Stair, Wall, Garbage, Punc, Id], [NewPlayer, Trash]) :- 
    refresh_rows([Stair, Wall], Punc, 4, [], [[NewStair, NewWall], FirstPunc, FirstTrash]),  

    take_garbage(Garbage, [NewGarbage, GTrash]),

    length(Trash, LT),
    (LT < 3 -> NewPunc is FirstPunc - LT;
        (LT < 6 -> (NewPunc is FirstPunc - 2 - ((LT - 2)*2));
            (LT < 9 -> (NewPunc is FirstPunc - 8 - ((LT - 5)*3));
                NewPunc is FirstPunc - 12))),
    
    append(FirstTrash, GTrash, Trash),
    NewPlayer = [NewStair, NewWall, NewGarbage, NewPunc, Id].

refresh_rows([Stair, Wall], Punctuation, 0, Trash, NewStWall) :- NewStWall = [[Stair, Wall], Punctuation, Trash], !.
refresh_rows([Stair, Wall], Punctuation, Row, Trash, NewStWall):-  
    nth0(Row, Stair, RowS),
    nth0(Row, Wall, RowW),
    findall(X,(member(X, RowS), X \= 0), Slabes),
    length(Stair, LS),
    
    (Slabes == LS -> 
        (nth0(0, RowS, Color), 
        indexOf(RowW, Color, Index),
        replace(Index, RowW, Color:1, NewRowW),

        calc_punctuation(Wall, [Row, Index], NPunctuation),
        NewPunctuation is Punctuation + NPunctuation,

        del(1, Slabes, Trs),
        findall(0, between(1, LS, _), NewRowS),  

        append(Trash, Trs, NewTrash),

        replace(Row, Stair, NewRowS, NewStair),
        replace(Row, Wall, NewRowW, NewWall));

        (NewWall = Wall,
        NewStair = Stair,
        NewPunctuation = Punctuation,
        NewTrash = Trash)
        ),
    
    NewR is Row - 1,
    refresh_rows([NewStair, NewWall], NewPunctuation, NewR, NewTrash, NewStWall).
    
%calcular la puntuacion al poner una ficha
calc_punctuation(Wall, [Row, Col], Punctuation) :- 
    matrix(Wall, Row, Col, Value),

    nth0(Row, Wall, WorkRow),
    findall(Value, matrix(Wall, _, Col, Value), WorkColumn),

    nth0(0, Wall, Row0),
    nth0(1, Wall, Row1),
    nth0(2, Wall, Row2),
    nth0(3, Wall, Row3),
    nth0(4, Wall, Row4),

    %cuenta la cantidad de azulejos contiguos hacia arriba
    check_minus(WorkColumn, Row, CountUp),
    %cuenta la cantidad de azulejos contiguos hacia abajo
    check_sum(WorkColumn, Row, CountDown),
    %cuenta la cantidad de azulejos contiguos hacia la izquierda
    check_minus(WorkRow, Col, CountLeft),
    %cuenta la cantidad de azulejos contiguos hacia la derecha
    check_sum(WorkRow, Col, CountRight),

    %chequea si se completo una fila y si esto ocurre, suma la bonificacion de 2 puntos
    (forall(member((_:X), WorkRow), X == 1) -> BonusRow = 2; BonusRow = 0),
    
    %chequea si se completo una columna y si esto ocurre, suma la bonificacion de 7 puntos
    (forall(member((_:X), WorkColumn), X == 1) -> BonusCol = 7; BonusCol = 0),

    %chequea si se completo una color en el tablero y si esto ocurre, suma la bonificacion de 10 puntos
    ((member(Value, Row0), 
    member(Value, Row1), 
    member(Value, Row2),
    member(Value, Row3),
    member(Value, Row4) -> (BonusColor = 10); BonusColor = 0)),

    %suma todos los puntos acumulados mas el punto de colocacion de la ficha
    Punctuation is CountUp + CountDown + CountLeft + CountRight + BonusRow + BonusCol + BonusColor + 1.

%chequea la cantidad de losas contiguas en una lista hacia la izquierda
check_minus(List, Position, Count) :- check_minus(List, Position, 0, Count).

check_minus(_, 0, C, Count) :- Count = C.
check_minus(List, Position, C, Count) :- 
    NewPosition is Position - 1,
    (nth0(NewPosition, List, Element), member((_:1), [Element] )) -> (NewC is C + 1, check_minus(List, NewPosition, NewC, Count)); Count = C, !.

%chequea la cantidad de losas contiguas en una lista hacia la derecha
check_sum(List, Position, Count) :- length(List, L), check_sum(List, Position, L, 0, Count).

check_sum(_, L, L, C, Count) :- Count = C.
check_sum(List, Position, L, C, Count) :- 
    NewPosition is Position + 1,
    (nth0(NewPosition, List, Element), member((_:1), [Element] )) -> (NewC is C + 1, check_sum(List, NewPosition, L, NewC, Count)); Count = C, !.


%actualiza el estado de la basura de un jugador luego de haber jugado una ronda
take_garbage(Player, Status) :-
    nth0(2, Player, Garbage),
    findall(X, (member(X, Garbage), X \= (_:0)), Slabs),
    findall(X, member((_:X), Slabs), Trash),
    garbage(Garbage),
    Status = [Garbage, Trash].


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