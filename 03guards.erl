-module('03guards').
-export([old_enough/1,
         right_age/1,
         wrong_age/1]).

old_enough(Age) when Age >= 18 -> true;
old_enough(_) -> false.

right_age(Age) when Age >= 21, Age =< 34 -> true;
right_age(_) -> false.

wrong_age(Age) ->
    not right_age(Age).
