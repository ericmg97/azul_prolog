%imprime el numero de ronda actual
print_round(Round) :-
    swritef(R, '______________RONDA %w______________', [Round]), nl, write(R), nl, nl.

%imprime el estado de la partida
print_status(Players) :-
    length(Players, P),
    print_players(Players, P).

print_status(Players, Factories) :- 
    length(Factories, F),
    length(Players, P),
    print_factories(Factories, F),
    print_players(Players, P).


%imprime el estado de las factorias
print_factories(Factories, 5) :-
    swritef(String, 'Fact1: %w   Fact2: %w   Fact3: %w   Fact4: %w   Fact5: %w' ,Factories),
    write(String), nl.
print_factories(Factories, 7) :-
    swritef(String, 'Fact1: %w   Fact2: %w   Fact3: %w   Fact4: %w   Fact5: %w   Fact6: %w   Fact7: %w' ,Factories),
    write(String), nl.
print_factories(Factories, 9) :-
    swritef(String, 'Fact1: %w   Fact2: %w   Fact3: %w   Fact4: %w   Fact5: %w   Fact6: %w   Fact7: %w   Fact8: %w   Fact9: %w' ,Factories),
    write(String), nl.
    
%imprime el estado de los jugadores
print_players(_, 0) :- !.
print_players(Players, I) :- 
    length(Players,L),
    Pl is L - I + 1,

    nth1(Pl, Players, P),
    nth0(4, P, Id),

    swritef(Head, '_________JUGADOR %w_________', [Id]),
    nl, write(Head), nl, nl,

    nth0(0, P, Stair),
    print_stair(Stair), nl,

    nth0(1, P, Wall),
    print_wall(Wall), nl,

    nth0(2, P, Garbage),
    print_garbage(Garbage), nl,
    
    nth0(3, P, Punt),
    swritef(Foot, 'Puntuacion: %w', [Punt]),
    write(Foot), nl, nl,
    
    X is I - 1,
    print_players(Players, X).

%imprime la escalera del jugador
print_stair(Stair) :- 
    write('Preparacion: '), nl,
    swritef(String, '%w \n%w \n%w \n%w \n%w', Stair),
    write(String), nl.

%imprime la pared del jugador
print_wall(Wall) :- 
    write('Pared: '), nl,
    swritef(String, '%w \n%w \n%w \n%w \n%w', Wall),
    write(String), nl.

%imprime el basurero del jugador
print_garbage(Garbage) :-
    swritef(String, 'Basura: %w', [Garbage]),
    write(String), nl.

%imprime los ganadores del juego
print_winners(Punctuation, Winners) :- 
    length(Winners, L),
    nth0(0, Winners, W),

    (L > 1 -> 
        swritef(Win,'LOS GANADORES SON: %w \n', [Winners]); 
        swritef(Win,'EL GANADOR ES EL JUGADOR %w \n', [W])),

    swritef(Punct, 'CON UNA PUNTUACION DE: %w', [Punctuation]),
    
    nl, write("FIN DE LA PARTIDA!!!"),nl,
    write(Win), 
    write(Punct), nl, nl.