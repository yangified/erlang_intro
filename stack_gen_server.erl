%% Declare our module
-module(stack_gen_server).

% This is a gen_server
-behaviour(gen_server).

% Declare the functions this module exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).
-export([push/1, pop/0, peek/0, reset/0]).

% Spawns and links a new process
start_link() ->
    gen_server:start_link({local, stack_gen_server}, stack_gen_server, [], []).

% Callback function if start_link/0 succeeds.
init([]) -> 
    {ok, []}.

% Callback functions for synchronous calls
handle_call({pop}, _From, Stack) ->
	[Item|Rest] = Stack,
    {reply, Item, Rest};

handle_call({peek}, _From, Stack) ->
    {reply, Stack, Stack};

handle_call({push, Item}, _From, Stack) ->
    {reply, ok, [Item | Stack]}.

% Callback fuunctions for async Calls
handle_cast({reset}, _Stack) ->
    {noreply, []}.

% Callback function to handle other messages
handle_info(_, Stack) ->
    {noreply, Stack}.

terminate(shutdown, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Library functions
push(Item) ->
    gen_server:call(stack_gen_server, {push, Item}).

pop() ->
    gen_server:call(stack_gen_server, {pop}).

peek() ->
    gen_server:call(stack_gen_server, {peek}).

reset() ->
    gen_server:cast(stack_gen_server, {reset}).
