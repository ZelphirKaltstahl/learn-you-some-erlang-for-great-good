-module(module01).
-export([add/2,
         hello/0]).

add(A, B) ->
    A + B.

%% Here is a comment.
%% io:format/1 is used for outputting text.
hello() ->
    io:format("Hello World!~n").
