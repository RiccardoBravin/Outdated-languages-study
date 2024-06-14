% 1. Define a “deep reverse” function, which takes a “deep” list, i.e. a list containing possibly lists of any
% depths, and returns its reverse.
% E.g. deeprev([1,2,[3,[4,5]],[6]]) is [[6],[[5,4],3],2,1].
% 2. Define a parallel version of the previous function.


-module('2023-07-03').
-compile(export_all).

deeprev(L) when
    is_list(L) -> lists:reverse([deeprev(X) || X <- L]);
    
deeprev(V) -> V.


pdeeprev(L) when is_list(L) -> 
    Me = self(),
    Pids = [spawn(?MODULE, pdeeprev, [X, Me]) || X <- L],
    Ans =  [receive {P, V} -> V end || P <- Pids],
    lists:reverse(Ans).

pdeeprev(L, Caller) when is_list(L) -> 
    Me = self(),
    Pids = [spawn(?MODULE, pdeeprev, [X, Me]) || X <- L],
    Ans =  [receive {P, V} -> V end || P <- Pids],
    Caller ! {self(), lists:reverse(Ans)};

pdeeprev(V, Caller) -> Caller ! {self(), V}.
