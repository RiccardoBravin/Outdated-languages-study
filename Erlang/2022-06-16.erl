% Define a program tripart which takes a list, two values x and y, with x < y, and three functions, taking
% one argument which must be a list.
% • tripart first partitions the list in three sublists, one containing values that are less than both x and
% y, one containing values v such that x ≤ v ≤ y, and one containing values that are greater than both
% x and y.
% • Three processes are then spawned in parallel, running the three given functions and passing the
% three sublists in order (i.e. the first function must work on the first sublist and so on).
% • Lastly, the program must wait the termination of the three processes in the spawning order,
% assuming that each one will return the pair {P, V}, where P is its PID and V the resulting value.
% • tripart must return the three resulting values in a list, with the resulting values in the same order
% as the corresponding sublists.

-module('2022-06-16').
-compile(export_all).

lt(List, X, Y) -> lists:filter(fun (V) -> ((V < X) and (V < Y))end, List).
btw(List, X, Y) -> lists:filter(fun (V) -> ((V >= X) and (V =< Y))end, List).
gt(List, X, Y) -> lists:filter(fun (V) -> ((V > X) and (V > Y))end, List).



tripart(List, X, Y, F, G, H) ->
    L1 = lt(List, X, Y),
    L2 = btw(List, X, Y),
    L3 = gt(List, X, Y),
    Pids = [spawn(?MODULE, F, [L1]), spawn(?MODULE, G, [L2]), spawn(?MODULE, H, [L3])],
    [receive {P, V} -> V end || P <- Pids].

