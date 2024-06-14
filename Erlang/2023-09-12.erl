% Consider the infinite list of binary trees of Exercise 2: instead of infinite lists, we want to create processes which
% return the current element of the "virtual infinite list" with the message next, and terminate with the message stop.
% 1. Define a function btrees to create a process corresponding to the infinite tree of Exercise 2.1.
% 2. Define a function incbtrees to create a process corresponding to the infinite tree of Exercise 2.2.
% Notes: for security reasons, processes must only answer to their creating process; to define trees, you can use
% suitable tuples with atoms as customary in Erlang (e.g. {branch, {leaf, 1}, {leaf, 1}}).


-module('2023-09-12').
-compile(export_all).

btrees(X, Caller) ->
    receive 
        {next} -> Caller ! {leaf, X},
                  btrees(X, {leaf, X}, Caller);
        {stop} -> Caller ! ok
    end.

btrees(X, Msg, Caller) ->
    receive 
        {next} -> Caller ! {branch, Msg, Msg},
                  btrees(X, {branch, Msg, Msg}, Caller);
        {stop} -> Caller ! ok
    end.


incbtree(Caller) ->
    receive 
        {next} -> Caller ! {leaf, 1},
                  incbtree(2, Caller);
        {stop} -> Caller ! ok
    end.

incbtree(X, Caller) ->
    Msg = buildtree(X, {leaf, X}),
    receive 
        {next} -> Caller ! Msg,
                  btrees(X + 1, Msg, Caller);
        {stop} -> Caller ! ok
    end.

buildtree(0, Msg) -> Msg;
buildtree(X, Msg) ->
    buildtree(X - 1, {branch, Msg, Msg}).
