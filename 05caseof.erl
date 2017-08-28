-module('05caseof').
-export([insert/2]).

insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X, Set) of
        true -> Set;
        false -> [X|Set]
    end.

beach_judgement(Temperature) ->
    case Temperature of
        {celsius, Degrees} when Degrees >= 20, Degrees =< 45 ->
            favorable;
        {kelvin, Degrees} when Degrees >= 293,
                               Degrees =< 318 ->
            'scientifically favorable';
        {fahrenheit, Degrees} when Degrees >= 68,
                                   Degrees =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.

%% In case expressions one can use custom functions, in contrast to guards.
%% The functions would be applied in place of Temperature in ~case Temperature of~.
%% For example: ~case is_high(Temperature) of~.
