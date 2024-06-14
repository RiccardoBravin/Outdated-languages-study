% We want to implement a parallel foldl, parfold(F, L, N), where the binary operator F is associative, and N
% is the number of parallel processes in which to split the evaluation of the fold. Being F associative,
% parfold can evaluate foldl on the N partitions of L in parallel. Notice that there is no starting (or
% accumulating) value, differently from the standard foldl.
% You may use the following libray functions:
% lists:foldl(<function>, <starting value>, <list>)
% lists:sublist(<list>, <init>, <length>), which returns the sublist of <list> starting at position
% <init> and of length <length>, where the first position of a list is 1.

-module('2022-09-01').
-compile(export_all).


divider([], _) ->[];
divider(List, N) -> [lists:sublist(List, 1, N)] ++ divider(lists:sublist(List, N + 1, length(List)), N).

fold_help(Caller, Fun, [H | T]) ->
    Me = self(),
    Caller ! {Me, lists:foldl(Fun, H, T)};

fold_help(Caller, Fun, [V]) ->
    Me = self(),
    Caller ! {Me, Fun(V)}.


parfold(Fun, List, Procs) ->
    Lists = divider(List, 1 + (length(List) div Procs)),
    Pids = [spawn(?MODULE, fold_help, [self(), Fun, L]) || L <- Lists],
    [A|Answers] = [receive {P, V} -> V end || P <- Pids],
    lists:foldl(Fun, A, Answers).


partition(L, N) ->
 M = length(L),
 Chunk = M div N,
 End = M - Chunk*(N-1),
 parthelp(L, N, 1, Chunk, End, []).

parthelp(L, 1, P, _, E, Res) ->
    Res ++ [lists:sublist(L, P, E)];

parthelp(L, N, P, C, E, Res) ->
    R = lists:sublist(L, P, C),
    parthelp(L, N-1, P+C, C, E, Res ++ [R]).