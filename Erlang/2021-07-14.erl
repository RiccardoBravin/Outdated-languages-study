% Consider a main process which takes two lists: one of function names, and one of lists of parameters (the
% first element of with contains the parameters for the first function, and so forth). For each function, the
% main process must spawn a worker process, passing to it the corresponding argument list. If one of the
% workers fails for some reason, the main process must create another worker running the same function.
% The main process ends when all the workers are done.

-module('2021-07-14').
-compile(export_all).


master(functions, parameters) ->
    register(master_spawner, self()),
    Workers = spawn_loop(functions, parameters, #{}),
    master_loop(Workers, length(functions)).

spawn_loop([],[], Map) -> Map;
spawn_loop([F|Fs], [P|Ps], Map) ->
    Pid = spawn_link(?MODULE, F, P),
    spawn_loop(Fs, Ps, Map#{Pid => {F, P}}).


master_loop(_, 0) -> true;
master_loop(Workers, Count) ->
    receive
        {'Exit', _, normal} -> %worker exited regularly
            master_loop(Workers, Count - 1);
        {'Exit', Worker, _} -> %worker crashed
            #{Worker := {F, P}} = Workers, %obtain function and starting parameters from dead worker
            Pid = spawn_link(?MODULE, F, P),
            master_loop(Workers#{Pid => {F,P}}, Count)
    end.