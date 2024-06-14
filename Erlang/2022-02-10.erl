% Define a parallel lexer, which takes as input a string x and a chunk TokenL n, and translates all the words in
% the strings to atoms, sending to each worker a chunk of x of TokenL n (the last chunk could be shorter than
% n). You can assume that the words in the string are separated only by space characters (they can be more
% than one - the ASCII code for ' ' is 32); it is ok also to split words, if they overlap on different chunks.
% E.g.
%  plex("this is a nice test", 6) returns [[this,i],[s,a,ni],[ce,te],[st]]
% For you convenience, you can use the library functions:
% • lists:sublist(List, Position, TokenL) which returns the sublist of List of TokenL TokenL from
% position Position (starting at 1);
% • list_to_atom(Word) which translates the string Word into an atom. 

-module('2022-02-10').
-compile(export_all).

% plex(String, Chunk) -> lexer(String, Chunk, 0).

% lexer(String, Chunk, Count) ->
%     if 
%     (length(String) > 0) and (Chunk > 0) ->
%         MyPid = self(),
%         spawn(?MODULE, tokenizer, [MyPid, Count, lists:sublist(String, 1, Chunk)]),
%         Current = lexer(lists:sublist(String, Chunk+1, length(String)),Chunk, Count + 1),
%         receive 
%             {Count, NewPart} -> [NewPart] ++ Current
%         end,
%         %print the partial result
%         io:format("Partial result: ~p~n", [Current]);
%     true ->
%         []
%     end.
    

% tokenizer(CallerPid, Count, String)->
%     Answer = tokenizer_loop(String, []),
%     CallerPid ! {Count, Answer}.

% tokenizer_loop([X | Xs], []) ->
%     if 
%         X =:= 32 -> tokenizer_loop(Xs, []);
%         X =/= 32 -> tokenizer_loop(Xs,[X])
%     end;

% tokenizer_loop([X|Xs], Word) ->
%     if 
%         X =:= 32 -> [list_to_atom(Word)] ++ tokenizer_loop(Xs, []);
%         X =/= 32 -> tokenizer_loop(Xs, Word ++ X)
%     end;

% tokenizer_loop([], []) -> [];
% tokenizer_loop([], Word) -> [list_to_atom(Word)].

%%CODICE DEL PROF ALTERNATIVO 

split(List, TokenL, Pos, End) when 
    Pos < End ->
        [lists:sublist(List, Pos, TokenL)] ++ split(List, TokenL, Pos+TokenL, End);
split(_, _, _, _) -> [].

lex([X|Xs], []) when X =:= 32 -> lex(Xs, []); % skip repeated spaces
lex([X|Xs], Word) when X =:= 32 -> [list_to_atom(Word)] ++ lex(Xs, []); % end of word
lex([X|Xs], Word) -> lex(Xs, Word++[X]); % add char to word
lex([], []) -> []; % end of string
lex([], Word) -> [list_to_atom(Word)]. % end of string


run(Pid, Data) -> Pid!{self(), lex(Data, [])}.

plex(List, TokenL) ->
 Part = split(List, TokenL, 1, length(List)), % section the list in chunks of size TokenL
 
 W = lists:map(fun(X) ->
                    spawn(?MODULE, run, [self(), X]) %spawn a worker which sends the result to the caller
                end, Part),
 lists:map(fun (P) ->
                receive
                    {P, V} -> V
                end
           end, W). %for each Pid in W reveive the result and save it in the list