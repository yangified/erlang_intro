%% Declare our module
-module(stack_server).

% Declare the functions this module exports
-export([start_link/0, peek/1, pop/1, push/2]).

% This will spawn a process of our server and return the Pid.
start_link() ->
    spawn_link(fun init/0).

% spawn_link calls this method to initialize a server
init() ->
    loop([]).

% The main event loop
loop(Stack) ->
    % Listen for messages
    receive
        {Pid, Ref, {pop}} ->
            [Item|Rest] = Stack,
            Pid ! {ok, Ref, Item},
            loop(Rest);
        {Pid, Ref, {peek}} ->
            Pid ! {ok, Ref, Stack},
            loop(Stack);
        {Pid, Ref, {push, Item}} ->
            Pid ! {ok, Ref},
            loop([Item | Stack]);
        Unknown ->
            loop(Stack)
    end.

% These are library methods to send a message to our hello_server and receive a response
push(Pid, Name) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {push, Name}},
    receive
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]);
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    end.

pop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {pop}},
    receive
        {ok, Ref, Item} ->
            erlang:demonitor(Ref, [flush]),
            io:format("Item: ~p~n", [Item]);
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    end,
    ok.

peek(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {peek}},
    receive
        {ok, Ref, Items} ->
            erlang:demonitor(Ref, [flush]),
            io:format("Items: ~p~n", [Items]);
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    end,
    ok.
