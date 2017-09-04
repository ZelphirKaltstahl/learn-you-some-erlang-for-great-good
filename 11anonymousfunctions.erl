-module('11anonymousfunctions').
-export([map/2]).

map(_, []) -> [];
map(Function, [H|T]) -> [Function(H) | map(Function, T)].

%% define anonymous functions as follows:
%% Fn = fun() -> a end. (in shell)

%% And use the following for multiple cases:
%% fun(Args1) ->
%%         Expression1, Exp2, ..., ExpN;
%%    (Args2) ->
%%         Expression1, Exp2, ..., ExpN;
%%    (Args3) ->
%%         Expression1, Exp2, ..., ExpN
%% end
