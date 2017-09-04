-module('20processes').
-export([initiate/0, calculate/2]).

loop(Func, N) when N > 0 ->
    Func(),
    loop(Func, N - 1);
loop(_Func, 0) ->
    io:format("Finished!~n").

calculate(Func, N) ->
    receive
        {SourceProcess, start} ->
            SourceProcess ! loop(Func, N),
            %% keep receiving!
            calculate(Func, N);
        {SourceProcess, _} ->
            SourceProcess ! undefined,
            %% keep receiving! even in case of not understood messages
            calculate(Func, N);
        _ ->
            error("Error: message not understood")
    end.

initiate() ->
    %% only spawn a the process once
    Calculator = spawn('20processes', calculate, [fun () -> 1+1 end, 1000]),

    %% give the process something to do!
    Calculator ! {self(), start},
    io:format("now waiting for response 1 ...~n"),
    receive
        undefined ->
            io:format("My message was not understood by the calculator :(~n");
        Result1 -> Result1
    end,

    %% give the process something to do!
    Calculator ! {self(), start},
    io:format("now waiting for response 2 ...~n"),
    receive
        undefined ->
            io:format("My message was not understood by the calculator :(~n");
        Result2 -> Result2
    end,

    %% give it a message it does not understand
    Calculator ! bla.
