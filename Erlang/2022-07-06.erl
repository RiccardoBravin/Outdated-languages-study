% We want to implement an interface to a server protocol, for managing requests that are lists of functions
% of one argument and lists of data on which the functions are called.
% The interface main function is called multiple_query and gets a list of functions FunL, and a list of data,
% DataL, of course of the same size.
% The protocol works as follows (assume that there is a registered server called master_server):
% 1. First, we ask the master server for a list of slave processes that will perform the computation. The
% request has the following form:
% {slaves_request, {identity, <id_of_the_caller>}, {quantity, <number_of_needed_slaves>}}
% 2. The answer has the following form:
% {slaves_id, <list_of_ids_of_slave_processes>}
% 3. Then, the library sends the following requests to the slave processes:
% {compute_request, {identity, <id_of_the_caller>}, <function>, <data>},
% where <function> is one of the elements of FunL, and <data> is the corresponding element of
% DataL.
% 4. Each process sends the result of its computation with a message:
% {compute_result, {identity, <slave_id>}, {value, <result_value>}}
% 5. multiple_query ends by returning the list of the computed results, that must be ordered
% according to FunL and DataL.
% If you want, you may use lists:zip/2 and lists:zip3/3, that are the standard zip operations on 2 or 3 lists,
% respectively.

-module('2022-06-16').
-compile(export_all).

%in sequence ask master server to create slaves = length(FunL), send to slaves for each function f and dataa and wait for results
%return the results in the same order of FunL and DataL
multiple_query(FunL, DataL) when
    length(FunL) == length(DataL) ->
        master_server ! {slaves_request, {identity, self()}, {quantity, length(FunL)}},
        receive 
            {slaves_id, Slaves} -> Slaves
        end,

        [Slave ! {compute_request, {identity, self()}, F, D} || {Slave, F, D} <- lists:zip(Slaves, FunL, DataL)],
        
        Answers = [receive {compute_result, {identity, Slave}, {value, R}} -> R end || Slave <- Slaves],
        Answers.