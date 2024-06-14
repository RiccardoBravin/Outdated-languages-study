-module(recap). % Name of the module must be the same as the file name
-compile(export_all). % Export all functions in the module 

% in console do "c(recap)." to compile the module and load it in the shell

% Note: the variables are written with a capital letter (Name) and the atoms with a lowercase letter (hello)

hello() -> % Function name (hello_world) and arity (0)
    "Hello world!". % Function body

hello(Name) -> % Function name (hello_world) and arity (1)
    "Hello " ++ Name ++ "!".

% in console or from another module do "recap:hello()." to call the function


% Guards
% Guards are used to test the values of the arguments passed to a function
% Guards are written after the function name and arity and are separated from the function body by a semicolon
% Guards are written between parentheses and are separated by commas
% Guards are evaluated from top to bottom

% 
safe_hello(Name) when is_list(Name) -> % Guard (is_list(Name)) to test that the argument (Name) is a list (string)
    "Hello " ++ Name ++ "!"; %we use a semicolon because we want to do something else if the guard fails
safe_hello(Name) when is_integer(Name) -> % Guard (is_integer(Name)) to test that the argument (Name) is an integer
    "Hello " ++ integer_to_list(Name) ++ "!"; 
safe_hello(_) -> % we use the underscore to match any value
    "Sorry, the value must be an integer or a string".

greet_adult(Name, Age) ->
    if 
        Age > 18 -> % if statement in which the conditions must be of the same type as the ones in the guards
            "Welcome back " ++ Name ++ "!";
        Age == 18 -> % else if statement
            "Welcome " ++ Name ++ "!";
        true -> % else statement
            "Sorry " ++ Name ++ ", you are too young to enter."
    end. % end of the if statement

% if we want to use other functions in the checks we must use the case statement
is_adult(Age) when Age >= 18 -> true;
is_adult(_) -> false.

greet_adult2(Name, Age) ->
    case is_adult(Age) of
        true -> "Welcome back " ++ Name ++ "!";
        _    -> "Sorry " ++ Name ++ ", you are too young to enter."
    end.

% lists
% lists are written between square brackets and the elements are separated by commas
% lists can contain any type of data
% lists can be concatenated using the ++ operator
how_long([]) -> 0;
how_long([_ | Tail]) -> 1 + how_long(Tail).

% maps
% maps are written between curly brackets and the elements are separated by commas
% maps can contain any type of data

%Works in console but not here for some reason
%Map = #{Key => Value}.
%Map = #{a => 0, b => 1, c => 3}. %define a map
%Map1 = Map#{d => 1}. %add a new key-value pair to the map
%Map2 = Map1#{a := 1}. %update the value of a keym


% touples
%{A, B, C} = {1, 2, 3}. %define a touple


% equality
% == and /= are used to test equality and inequality
% =:= and =/= are used to test equality and inequality in a strict way (the types must be the same)

% APPLY
% apply is used to call a function dynamically
% apply(Module, Function, Arguments)

% apply(recap, hello, ["John"]). % call the function hello from the module recap with the argument "John"



%%% CONCURRENCY

%ROCK PAPER SCISSORS
game_start() ->
    io:format("Welcome to the game!~n"), %basic print function (~n is a newline)
    io:format("I'm process ~p~n", [self()]), %print the pid of the current process (self() returns the pid of the current process)
    Pid = spawn(?MODULE, main_loop, []), % spawn a process that will execute the function main_loop with no arguments (?MODULE is a macro that returns the name of the current module)
    %alternative: Pid = spawn(fun() -> main_loop() end). % spawn a process using a lambda function as code (the Pid is the child process)
    io:format("Spawned process ~p~n", [Pid]),
    Pid ! {self(), rock}, % send a message to the process Pid (the message is a touple containing the pid of the current process and the value rock)
    Pid ! {self(), paper},
    Pid ! {self(), scissors}, %send multiple messages in non blocking mode

    receiver_loop(). % call the function receiver_loop which will wait for a message forever


receiver_loop() ->
    receive
        Message -> io:format("The game was a ~p~n", [Message])
    after 1000 -> exit(ok) %after 2 seconds the process will exit with the reason ok
    end,
    receiver_loop(). %loop the function to wait for more messages

main_loop() -> 
    io:format("CPU player ready!~n"),
    Choices = [rock, paper, scissors], %list of possible choices 
    CPU_choice = lists:nth(rand:uniform(3), Choices), %pick a random element from the list
    receive
        {PlayerPid, PlayerChoice} -> PlayerPid ! win_check(PlayerChoice, CPU_choice) %match the message and send the result of the game to the player
    end,
    main_loop(). %loop the function to play again



win_check(PlayerChoice, CPU_choice) ->
    case {PlayerChoice, CPU_choice} of
        {rock, paper} -> win;
        {rock, scissors} -> lose;
        {paper, rock} -> win;
        {paper, scissors} -> lose;
        {scissors, rock} -> lose;
        {scissors, paper} -> win;
        {_, _} -> draw
    end.


%let's see the link between processes 

bad_proc() -> io:format("I'm a bad process!~n"), 4/0. %make the process crash


test_spawn() -> 
    process_flag(trap_exit, true), %set the trap_exit flag to true to receive exit messages instead of crashing 
    %spawn(?MODULE, bad_proc, []), %spawn a process that will crash but we will not receive the exit message because we are not linked to it
    spawn_link(?MODULE, bad_proc, []), %spawn a process that will crash and we will receive the exit message because we are linked to it
    receive
        Err -> io:format("The process crashed with the reason ~p~n", [Err])
    end.        


%another example of link between processes (the child process will die if the parent process dies)
dying_parent(N) -> 
    %Pid = spawn_link(fun() -> child(N) end), %with spawn_link the parent process will die after 3 seconds and the child process will die too
    Pid = spawn(fun() -> child(N) end),  %with spawn the parent process will die after 3 seconds and the child process will continue to live
    io:format("Parent process ~p spawned child process ~p~n", [self(), Pid]),
    receive 
        after 3000 -> io:format("Parent process ~p died~n", [self()]), exit(ok)
    end.

child(0) -> io:format("Child process ~p ended~n", [self()]), exit(ok);
child(N) -> 
    receive 
        after 500 -> io:format("Still alive~n")
    end,
    child(N-1).

