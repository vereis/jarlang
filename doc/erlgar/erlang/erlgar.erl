-module(erlgar).
-export([
    init/0,         %Initiallizes the game & interface
    update/0         %update the game
    ]).
%%% ---------------------------------------------------------------------------------------------%%%
%%% - Agar game in erlang -----------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

init()->
    register(game,self()),
    [Width,Height] = gameArea:getCanvasSize(),
    loop(Width,Height).

loop(Width,Height)->
    receive
        tick ->
            gameArea:clear(),
            [PrevX,PrevY] = gameArea:getMousePos(),
            [PlayerX,PlayerY]=get_new_player_pos(Width,Height,PrevX,PrevY,50),
            gameArea:draw(10,10,"Red",PlayerX-5,PlayerY-5,"square"),
            loop(Width,Height)
    end.

update()->
    game ! tick.


get_new_player_pos(Width,Height,PrevX,PrevY,Speed)->
    Dx=PrevX-Width/2,
    Dy=PrevY-Height/2,
    M = vec_mag(Dx,Dy),
    [Width/2+(Dx/M)*Speed,Height/2+(Dy/M)*Speed].

vec_mag(X,Y)->
    sqrt(X*X+Y*Y).

% Implement sqrt because we haven't coded the math module yet
sqrt(N)->
    sqrt(N,N/2).
sqrt(N,G)->
    D = N/G,
    NG = (G+D)/2,
    case NG of
        G -> G;
        _ -> sqrt(N,NG)
    end.