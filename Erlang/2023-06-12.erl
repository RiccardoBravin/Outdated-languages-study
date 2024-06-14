

-module('2023-06-12').
-compile(export_all).


% Consider the following implementation of mergesort and write a parallel version of it.
mergesort([L]) -> [L];
mergesort(L) ->
 {L1, L2} = lists:split(length(L) div 2, L),
 merge(mergesort(L1), mergesort(L2)).

merge(L1, L2) -> merge(L1, L2, []).
merge([], L2, A) -> A ++ L2;
merge(L1, [], A) -> A ++ L1;
merge([H1|T1], [H2|T2], A) when H2 >= H1 -> merge(T1, [H2|T2], A ++ [H1]);
merge([H1|T1], [H2|T2], A) when H1 > H2 -> merge([H1|T1], T2, A ++ [H2]).

pmergesort([L]) -> [L];
pmergesort(L) -> 
    Me = self(),
    {L1, L2} = lists:split(length(L) div 2, L),
    PidL = spawn(?MODULE, pmergesort, [L1, Me]),
    PidR = spawn(?MODULE, pmergesort, [L2, Me]),

    receive 
        {PidL, Ll} -> receive 
                        {PidR, Rl} -> merge(Ll,Rl)
                      end
    end.

pmergesort([L], Caller) -> Caller ! {self(),[L]};
pmergesort(L, Caller) -> 
    Me = self(),
    {L1, L2} = lists:split(length(L) div 2, L),
    PidL = spawn(?MODULE, pmergesort, [L1, Me]),
    PidR = spawn(?MODULE, pmergesort, [L2, Me]),

    receive 
        {PidL, Ll} -> receive 
                        {PidR, Rl} -> Caller ! {self(), merge(Ll,Rl)}
                      end
    end.



