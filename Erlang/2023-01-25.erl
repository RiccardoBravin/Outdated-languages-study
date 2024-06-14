%build the drawing of the automaton

-module('2023-01-25').
-compile(export_all).



q0_f() ->
    receive
        {string, [97|T], S} -> q0 ! {string, T, S};
        {string, [98|T], S} -> q1 ! {string, T, S},
                              q2 ! {string, T, S}
    end,
    q0_f().

q1_f() ->
    receive
        {string, [98|T], S}-> q0 ! {string, T, S}
    end,
    q1_f().

q2_f() ->
    receive
        {string, [98|T], S}-> q3 ! {string, T, S}
    end,
    q2_f().


q3_f() ->
    receive
        {string, [99|T], S}-> q4 ! {string, T, S}
    end,
    q3_f().    


q4_f() ->
    receive
        {string, [], S}-> io:format("La stringa ~p è riconosciuta~n", [S])
    end,
    q4_f().


starter() ->
    register(q0, spawn(fun () -> q0_f() end)),
    register(q1, spawn(fun () -> q1_f() end)),
    register(q2, spawn(fun () -> q2_f() end)),
    register(q3, spawn(fun () -> q3_f() end)),
    register(q4, spawn(fun () -> q4_f() end)).

check(S) ->
    q0 ! {string, S, S},
    ok.

main()->
    check("Ciao"),
    check("aabbbbc").
    