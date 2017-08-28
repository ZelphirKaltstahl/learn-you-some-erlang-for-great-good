-module('08quicksort').
-export([quicksort/1]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smallers,Largers} = partition(Pivot, Rest, [], []),
    quicksort(Smallers) ++ [Pivot] ++ quicksort(Largers).

partition(_, [], Smallers, Largers) -> {Smallers, Largers};
partition(Pivot, [H|T], Smallers, Largers) ->
    if H =< Pivot -> partition(Pivot, T, [H|Smallers], Largers);
       H > Pivot -> partition(Pivot, T, Smallers, [H|Largers])
    end.
