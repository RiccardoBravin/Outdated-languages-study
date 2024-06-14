-module(ex1).
-compile(export_all).

%%%Define a "deep_reverse" function which takes a deep list and returns the reverse of it.
%deeprev([1,2,[3,[4,5]],[6]]) -> [[6],[[5,4],3],2,1].
%%%Define then the parallelized version of it.


%deeprev([]) -> []; %Not needed because the last case catches it
deeprev([H|T]) -> deeprev(T) ++ [deeprev(H)];
deeprev(X) -> X.



deeprev_par(List) ->
    Self = self(),
    %spawn a process that will execute the function deeprev_par with the arguments List and Self
    spawn_link(?MODULE, deeprev_par, [List, Self]),
    
    receive
        {Self, Res} -> Res
    end.


deeprev_par([H|T], Pid) ->
    Self = self(),
    %spawn a process to work on the head of the list
    spawn_link(?MODULE, deeprev_par, [H, Self]),

    %spawn a process to work on the tail of the list
    spawn_link(?MODULE, deeprev_par, [T, Self]),
    
    receive
        {Self, Sx} -> 
            receive
                {Self, Dx} -> Pid ! {Self, Dx ++ [Sx]}
            end
    end;


deeprev_par(X, Pid) -> 
    Pid ! {Pid, X}.


