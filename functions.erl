-module(functions).
-export([head/1,
         second/1,
         same/2]).

head([H|_]) ->
    H.

second([_,Second|_]) ->
    Second.

same(A, A) ->
    true;
same(_, _) ->
    false.
