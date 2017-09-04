-module('15rpncalc').
-export([rpn/1,
         rpn_test/0,
         input_to_tokens/1]).

%% RPN stands for "reverse polish notation"
%% Example: 1 2 + 4 5 6 * - / => 3 4 5 6 * - / => 3 4 30 - / => 3 -26 / => 3 over -26

string_to_number(String) ->
    case string:to_float(String) of
        {error, no_float} -> list_to_integer(String);
        {F, _} -> F
    end.

input_to_tokens(Input) -> string:tokens(Input, " ").


rpn(Input) when is_list(Input) ->
    [Result] = lists:foldl(fun rpn/2, [], input_to_tokens(Input)),
    Result.

rpn("+", [X1, X2 | Stack]) -> [X1 + X2 | Stack];
rpn("+", [_]) -> error(badarith);
rpn("+", []) -> error(badarith);
rpn("-", [X1, X2 | Stack]) -> [X2 - X1 | Stack];
rpn("-", [_]) -> error(badarith);
rpn("-", []) -> error(badarith);
rpn("*", [X1, X2 | Stack]) -> [X1 * X2 | Stack];
rpn("*", [_]) -> error(badarith);
rpn("*", []) -> error(badarith);
rpn("/", [X1, X2 | Stack]) -> [X2 / X1 | Stack];
rpn("/", [_]) -> error(badarith);
rpn("/", []) -> error(badarith);
rpn("^", [N1, N2 | Stack]) -> [math:pow(N2, N1) | Stack];
rpn("^", [_]) -> error(badarith);
rpn("^", []) -> error(badarith);
rpn("ln", [N | Stack]) -> [math:log(N) | Stack];
rpn("ln", []) -> error(badarith);
rpn("log10", []) -> error(badarith);
rpn("log10", [N | Stack]) -> [math:log10(N) | Stack];
rpn("sum", [_]) -> error(badarith);
rpn("sum", Stack) -> [lists:foldl(fun (A, B) -> A + B end, 0, Stack)];
rpn("prod", [_]) -> error(badarith);
rpn("prod", Stack) -> [lists:foldl(fun (A, B) -> A * B end, 1, Stack)];

rpn(X, Stack) -> [read(X) | Stack].
read(N) ->
    try
        string_to_number(N)
    catch
        Exception:Reason -> {Exception, caught, Reason}
    end.

rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try
             rpn("90 34 12 33 55 66 + * - +")
         catch
             error:{badmatch,[_|_]} -> ok
         end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.
