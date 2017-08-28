-module('04if').
-export([heh_fine/1,
         help_me/1]).

heh_fine(X) ->
    if 1 =:= 1 -> works end,
    if 1 =:= 2; 1 =:= 1 -> works end,
    if X =:= 10 -> might_succeed;
       true -> else_succeeds  %% else branch
    end.

help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == beef -> "mooo";
              Animal == dog -> "bark";
              Animal == tree -> "bark";
              true -> "asdasdasd"
           end,
    {Animal, "says " ++ Talk ++ "!"}.
