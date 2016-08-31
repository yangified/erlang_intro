% Declare our module
-module(hello).

% Declare the functions this module exports
-export([add/2, add/3, old_enough/1, push/2, pop/1, peek/1]).

add(A, B) ->
    A + B.

add(A, B, C) ->
    A + B + C.

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

push(Stack, Item) ->
    [Item | Stack].

pop(Stack) ->
    [Item | Rest] = Stack,
    {Item, Rest}.

peek(Stack) ->
    Stack.
