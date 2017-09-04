-module('21processes2').
-export([loop/2,
         fridge/1,
         start/1,
         store/2,
         take/2,
         process_message/2]).

loop(Func, N) when N > 0 ->
    Func(),
    loop(Func, N - 1);
loop(_Func, 0) ->
    io:format("Finished!~n").



fridge(FoodList) ->
    receive
        {SourceProcess, {store, Food}} ->
            io:format("got request to store food~n"),
            SourceProcess ! {self(), ok},
            fridge([Food|FoodList]);
        {SourceProcess, {take, Food}} ->
            io:format("got request to take food~n"),
            case lists:member(Food, FoodList) of
                true -> SourceProcess ! {self(), {ok, Food}},
                        fridge(lists:delete(Food, FoodList));
                false -> SourceProcess ! {self(), not_found},
                         fridge(FoodList)
            end;
        terminate ->
            ok
    end.

start(InitialFoodList) ->
    spawn(?MODULE, fridge, [InitialFoodList]).

store(Pid, Food) ->
    process_message(Pid, {store, Food}).

take(Pid, Food) ->
    process_message(Pid, {take, Food}).

process_message(Pid, SentMessage) ->
    Pid ! {self(), SentMessage},
    receive
        {Pid, Message} -> Message;
        Other -> Other
    end.
