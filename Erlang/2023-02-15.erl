
% build the non deterministic automaton with stack 


-module('2023-02-15').
-compile(export_all).

q0() ->
    receive 
        {[97|InT], [90|StackT], S} -> q1 ! {InT, "AZ" ++ StackT, S}
    end.

q1() ->
    receive 
        {[97|InT], [65|StackT], S} -> q1 ! {InT, "AA" ++ StackT, S};
        {[98|InT], [65|StackT], S} -> q2 ! {InT, StackT, S},
                                      q3 ! {InT, "A" ++ StackT, S}
    end.

q2() ->
    receive 
        {[98|InT], [65|StackT], S} -> q2 ! {InT, StackT, S};
        {[], [90|StackT], S} -> q5 ! {[], StackT, S}
    end.

q3() ->
    receive 
        {[98|InT], [65|StackT], S} -> q4 ! {InT, StackT, S}
    end.


q4() ->
    receive 
        {[98|InT], [65|StackT], S} -> q3 ! {InT, "A" ++ StackT, S};
        {[], [90|StackT], S} -> q5 ! {[], StackT, S}
    end.

q5()-> 
    receive 
        {[], [], S}-> io:format("La stringa ~p è riconosciuta~n", [S])
    end.

startup()->
    register(q0, spawn(fun() -> q0() end)),
    register(q1, spawn(fun() -> q1() end)),
    register(q2, spawn(fun() -> q2() end)),
    register(q3, spawn(fun() -> q3() end)),
    register(q4, spawn(fun() -> q4() end)),
    register(q5, spawn(fun() -> q5() end)).
    
check(S)->
    q0 ! {S, "Z", S}.
