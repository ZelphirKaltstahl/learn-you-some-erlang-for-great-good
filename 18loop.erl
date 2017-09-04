-module('18loop').
-export([loop/2]).

loop(Func, N) when N > 0 ->
    Func(),
    loop(Func, N - 1);
loop(_Func, 0) ->
    io:format("Finished!~n").
