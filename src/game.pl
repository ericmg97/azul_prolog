%inicializar la partida dependiendo de la cantidad de jugadores
play(N) :- (N =:= 2 -> game2) ; (N =:= 3 -> game3) ; game4.

game2 :- board(B1), board(B2), init_game([B1,0,[]], [B2, 0, []], 5, Container, ).
game3 :- board(B1), board(B2), board(B3), init_game([B1,0,[]], [B2, 0, []], [B3, 0, []]).