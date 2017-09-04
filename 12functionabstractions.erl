-module('12functionabstractions').
-export([map/2,
         fold/3,
         min/1,
         max/1,
         sum/1,
         reverse/1,
         map_with_fold/2,
         filter_with_fold/2]).

map(_, []) -> [];
map(Function, [H|T]) -> [Function(H) | map(Function, T)].

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

min([Head|Tail]) ->
    fold(fun (A,B) when B < A -> B;
             (A,_) -> A end,
         Head,
         Tail).

max([Head|Tail]) ->
    fold(fun (A,B) when B > A -> B;
             (A,_) -> A end,
         Head,
         Tail).

sum(List) ->
    fold(fun (A,B) -> A + B end,
         0,
         List).

reverse(List) ->
    fold(fun (X, Acc) -> [X|Acc] end,
         [],
         List).

map_with_fold(Func, List) ->
    reverse(fold(fun (X, Acc) -> [Func(X)|Acc] end,
                 [],
                 List)).

filter_with_fold(Pred, List) ->
    reverse(fold(fun (X, Acc) ->
                         case Pred(X) of
                             true -> [X|Acc];
                             false -> Acc
                         end
                 end,
                 [],
                 List)).
