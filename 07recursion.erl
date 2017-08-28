-module('07recursion').
-export([bad_multiply_item/2,
         multiply_item/2,
         multiply_item/3,
         bad_reverse/1,
         reverse/1,
         sublist/2,
         zip/2]).

bad_multiply_item(0,_) ->
    [];
bad_multiply_item(N,Term) ->
    [Term | bad_multiply_item(N-1, Term)].

%% when the number of arguments is different,
%% we actually have another function and the .
%% is appropriate for ending the definition.
multiply_item(N,Term) ->
    multiply_item(N,Term,[]).
multiply_item(0,_,List) ->
    List;
multiply_item(N,Term,List) when N > 0 ->
    multiply_item(N-1,Term,[Term|List]).


bad_reverse([]) -> [];
bad_reverse([H|T]) ->
    %% not tail recursive --> stacks up on stack
    %% uses append operation, which needs to traverse whole list
    bad_reverse(T)++[H].

reverse(L) -> tr_reverse(L,[]).
tr_reverse([], Acc) -> Acc;
%% "cons"es the head as head, so does not use append
tr_reverse([H|T], Acc) -> tr_reverse(T, [H|Acc]).

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist(List, N) -> tr_sublist(List, N, []).
tr_sublist(_List, 0, Acc) -> reverse(Acc);
tr_sublist([H|T], N, Acc) -> tr_sublist(T, N-1, [H|Acc]).


zip(L1, L2) -> reverse(tr_zip(L1, L2, [])).
tr_zip([], _, Acc) -> Acc;
tr_zip(_, [], Acc) -> Acc;
tr_zip([], [], Acc) -> Acc;
tr_zip([X | Xs], [Y | Ys], Acc) ->
    tr_zip(Xs, Ys, [{X, Y} | Acc]).
