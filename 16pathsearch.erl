-module('16pathsearch').
-export([main/1]).

%% run this program with:
%% erl -noshell -run 16pathsearch main road.txt

main([FilePath]) ->
    {ok, BinaryFileContent} = file:read_file(FilePath),
    Map = parse_map(BinaryFileContent),
    %% print result
    io:format("~p~n",[optimal_path(Map)]),
    %% shutdown the Erlang VM
    erlang:halt().

%% First we get our data into a more suitable representation of 3-tuples.
%% file => binary content => list (string) => list of integers of cost for segments
%% then group the list of integers into 3-tuples
parse_map(BinaryFileContent) when is_binary(BinaryFileContent) ->
    parse_map(binary_to_list(BinaryFileContent));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X) || X <- string:tokens(Str, " \r\n\t")],
    three_tuples(Values, []).

%% groups into 3-tuples
three_tuples([], Acc) ->
    lists:reverse(Acc);
three_tuples([A, B, X | Rest], Acc) ->
    three_tuples(Rest, [{A, B, X} | Acc]).

%% selects the shortest steps to the next 2 starting points
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
    %% possibly optimal paths to the next intersection A
    %% use A, so keep path to previous A
    OptionA1 = {DistA + A,
                [{a, A} | PathA]},
    %% use B and X, so keep path to previous B
    OptionA2 = {DistB + B + X,
                [{x, X}, {b, B} | PathB]},
    %% possibly optimal paths to the next intersection B
    %% use B, so keep path to previous B
    OptionB1 = {DistB + B,
                [{b, B} | PathB]},
    %% use A and X, so keep path to previous A
    OptionB2 = {DistA + A + X,
                [{x, X}, {a, A} | PathA]},
    %% taking the min works because the distance is the first element in the option tuples
    {erlang:min(OptionA1, OptionA2), erlang:min(OptionB1, OptionB2)}.

%% We need to traverse all 3-tuples of segments.
%% We can use a fold for this.
optimal_path(Map) ->
    InitialPaths = {{0, []}, {0, []}},
    {BestPathA, BestPathB} = lists:foldl(fun shortest_step/2, InitialPaths, Map),
    %% Both paths have the same length at the end, because they already consider
    %% the last step through the X = 0 segment, to the next intersection points.
    %% Because the last X = 0, and because we select the shortest way to the intersections,
    %% only the prefix path which was shorter in the previous step i.e. to the previous 2
    %% intersections will "survive" and the X = 0 will be once added and once not added to
    %% get to the last intersections.
    %% Then we need to figure out, which one does not contains the redundant X = 0 segment
    %% and use that as the shortest path.
    {_Dist, Path} = if hd(element(2, BestPathA)) =/= {x, 0} ->
                            BestPathA;
                       hd(element(2, BestPathB)) =/= {x, 0} ->
                            BestPathB
                    end,
    lists:reverse(Path).
