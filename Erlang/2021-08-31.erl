% Define a function which takes two list of PIDs [x1, x2, ...], [y1, y2, ...], having the same length, and a
% function f, and creates a different "broker" process for managing the interaction between each pair of
% processes xi and yi.
% At start, the broker process i must send its PID to xi and yi with a message {broker, PID}. Then, the
% broker i will receive messages {from, PID, data, D} from xi or yi, and it must send to the other one an
% analogous message, but with the broker PID and data D modified by applying f to it.
% A special stop message can be sent to a broker i, that will end its activity sending the same message to xi
% and yi. 

-module('2021-08-31').
-compile(export_all).

broker(Pidx, Pidy, F) ->
    Pidx ! {broker, self()},
    Pidy ! {broker, self()},
    broker_loop(Pidx, Pidy, F).
    

broker_loop(Pidx, Pidy, F) ->
    receive 
        {from, Pidx, data, D} -> 
            Pidy ! {from, self(), data, apply(?MODULE, F, D)}, 
            broker_loop(Pidx, Pidy, F); %send msg with self() PID and function applied to data to Pidy
        {from, Pidy, data, D} -> 
            Pidx ! {from, self(), data, apply(?MODULE, F, D)}, 
            broker_loop(Pidx, Pidy, F); %send msg with self() PID and function applied to data to Pidy
        {terminate} -> Pidx ! {terminate}, Pidy ! {terminate} %do not call loop anymore, just exit
    end.


broker_spawner([], _, _) -> true;
broker_spawner(_, [], _) -> true;
broker_spawner([Pidx | Xs], [Pidy | Ys], Function) -> 
    spawn(?MODULE, broker, [Pidx, Pidy, Function]),
    broker_spawner(Xs, Ys, Function).
