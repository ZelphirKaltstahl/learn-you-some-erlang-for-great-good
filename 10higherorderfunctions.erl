-module('10higherorderfunctions').
-export([one/0,
         two/0,
         add/2,
         increment/1,
         decrement/1,
         map/2,
         inc/1,
         dec/1]).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().
%% call with:
%% fun Module:Function/Arity

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

map(_, []) -> [];
map(Function, [H|T]) -> [Function(H) | map(Function, T)].

inc(X) ->
    X + 1.
dec(X) ->
    X - 1.
