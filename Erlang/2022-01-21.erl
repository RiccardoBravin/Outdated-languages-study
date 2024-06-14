% Create a distributed hash table with separate chaining. The hash table will consist of an agent for each
% bucket, and a master agent that stores the buckets’ PIDs and acts as a middleware between them and the
% user. Actual key/value pairs are stored into the bucket agents.
% The middleware agent must be implemented by a function called hashtable_spawn that takes as its
% arguments (1) the hash function and (2) the number of buckets. When executed, hashtable_spawn
% spawns the bucket nodes, and starts listening for queries from the user. Such queries can be of two kinds:
% • Insert: {insert, Key, Value} inserts a new element into the hash table, or updates it if an
% element with the same key exists;
% • Lookup: {lookup, Key, RecipientPid} sends to the agent with PID “RecipientPid” a
% message of the form {found, Value}, where Value is the value associated with the given key, if
% any. If no such value exists, it sends the message not_found.


%un thread per bucket, un master che tiene a memoria i bucket e trasmette a questi i segnali

-module('2022-01-21').
-compile(export_all).

%prende la funzione di hash e il numero di bucket (thread to spawn)
%gestire query: Insert: {insert, Key, Value} invia al bucket corretto la coppia chiave valore da inserire 
%               Lookup: {lookup, Key, RecipientPid} chiede al bucket corretto di controllare se ha un valore per la chiave 
%                       lo ritorna se trovato al RecipientPid altrimenti manda mess not_found 
hashtable_spawn(Hash, Count) ->
    Pids = [spawn(?MODULE, bucket, [#{}]) || _ <- lists:seq(0, Count)], %launch Count processes saving each Pid inside the list 
    hashtable_loop(Hash, Pids).

hashtable_loop(Hash, Buckets) ->
    receive
        {insert, Key, Value} -> 
            Pid = lists:nth(Hash(Key) + 1, Buckets),
            Pid ! {insert, Key, Value};
        {lookup, Key, RecipientPid} -> 
            Pid = lists:nth(Hash(Key) + 1, Buckets),
            Pid ! {lookup, Key, RecipientPid}
    end,

    hashtable_loop(Hash, Buckets).
    
bucket(Content) -> 
    receive
        {insert, Key, Value} -> bucket(Content#{Key => Value});
        {lookup, Key, RecipientPid} ->
            try  
                #{Key := Value} = Content,
                RecipientPid ! {found, Value}
            catch error:badkey ->
                RecipientPid ! not_found
            end,
            bucket(Content)
    end.
    

% The following code:
main() ->
    HT = spawn(?MODULE, hashtable_spawn, [fun(Key) -> Key rem 7 end, 7]),
    HT ! {insert, 15, "Apple"},
    HT ! {insert, 8, "Orange"},
    timer:sleep(500),
    HT ! {lookup, 8, self()},
 
    receive
        {found, A1} -> io:format("~s~n", [A1])
    end,
    HT ! {insert, 8, "Pineple"},
    HT ! {insert, 8, "Pineapple"},
    timer:sleep(500),
 
    HT ! {lookup, 8, self()},
    receive
        {found, A2} -> io:format("~s~n", [A2])
    end.

% should print the following:
% Orange
% Pineapple

