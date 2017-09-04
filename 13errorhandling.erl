-module('13errorhandling').
-export([some_handling/1,
         some_handling2/1,
         catcher/2]).

%% The pattern for handling errors is:

%% try Expression of
%%     SuccessfulPattern1 [Guards] ->
%%         Expression1;
%%     SuccessfulPattern2 [Guards] ->
%%         Expression2
%% catch
%%     TypeOfError:ExceptionPattern1 ->
%%         Expression3;
%%     TypeOfError:ExceptionPattern2 ->
%%         Expression4
%% end.

some_handling(F) when is_function(F, 0) ->
    try
        %% multiple expressions possible here
        io:format("I will now run F()!~n"),
        F()
    of
        %% If there was an exception or an error or an exit inside the function
        %% F, it would not have had the chance to return anything.
        %% This means, that if we get any return value at all, the function did
        %% not throw an exception, cause an error or exit.
        _ -> ok
    catch
        %% catch specific exception
        throw:'permission denied' -> {throw, caught, "Permission has been denied!"};
        %% catch all exceptions
        throw:SomethingThrown -> {throw, caught, SomethingThrown};
        error:SomeError -> {error, caught, SomeError};
        exit:AnExit -> {exit, caught, AnExit};
        %% catch any other exception or error or exit
        %% this might not be a good practice
        Exception:Reason -> {Exception, caught, Reason}
        %% _:_ -> "Some other exception / error / exit occured."
    after  % is guaranteed to be executed
        io:format("exceptions, errors and exits have been dealt with!~n")
    end.

%% The `of` part can be left out.
some_handling2(F) when is_function(F, 0) ->
    try
        %% multiple expressions possible here
        io:format("I will now run F()!~n"),
        F()
    catch
        %% catch specific exception
        throw:'permission denied' -> {throw, caught, "Permission has been denied!"};
        %% catch all exceptions
        throw:SomethingThrown -> {throw, caught, SomethingThrown};
        error:SomeError -> {error, caught, SomeError};
        exit:AnExit -> {exit, caught, AnExit};
        %% catch any other exception or error or exit
        %% this might not be a good practice
        Exception:Reason -> {Exception, caught, Reason}
        %% _:_ -> "Some other exception / error / exit occured."
    after  % is guaranteed to be executed
        io:format("exceptions, errors and exits have been dealt with!~n")
    end.

catcher(X, Y) ->
    case catch X/Y of
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.
