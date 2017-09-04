-module('14treewithexceptions').
-export([empty/0,
         insert/3,
         lookup/2,
         has_value_naive/2,
         has_value_throws/2]).

%% nodes are tuples:
%% - node (tag, denoting the type of the tuple)
%% - key
%% - value
%% - smaller and
%% - larger
%% structured as follows: {node {key value smaller larger}}

%% used as an initial root node
empty() ->
    {node, nil}.
%% if we insert at an empty node (base case)
insert(Key,
       Value,
       {node, nil}) ->
    {node, {Key, Value, empty(), empty()}};

insert(NewKey,
       NewValue,
       {node, {Key, Value, Smaller, Larger}})
  when NewKey < Key ->
    {node, {Key, Value, insert(NewKey, NewValue, Smaller), Larger}};

insert(NewKey,
       NewValue,
       {node, {Key, Value, Smaller, Larger}})
  when NewKey > Key ->
    {node, {Key, Value, Smaller, insert(NewKey, NewValue, Larger)}}.

lookup(_, {node, nil}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    Val;
lookup(Key, {node, {NodeKey, _NodeValue, Smaller, _Larger}})
  when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_NodeKey, _NodeValue, _Smaller, Larger}}) ->
    lookup(Key, Larger).

%% at an empty node, the value cannot exist
has_value_naive(_, {node, nil}) -> false;
%% if the value matches the value in the node, we found it
has_value_naive(Value, {node, {_, Value, _, _}}) -> true;
has_value_naive(Value, {node, {_, _, Right, Left}}) ->
    %% first go left until you find an empty node or node with matching value
    %% depth first search
    case has_value_naive(Value, Left) of
        true -> true;
        false -> has_value_naive(Value, Right)
    end.

%% now a version with throwing
%% uses non-local return implemented by using throw
%% this can have speed advantages for deep trees (although probably neglectable)
has_value_throws(Val, Tree) ->
    try has_value(Val, Tree) of
        false -> false
    catch
        %% if at any point in the search 'found' is thrown
        %% return true
        throw:found -> true
    end.

has_value(_, {node, 'nil'}) -> false;
has_value(Val, {node, {_, Val, _, _}}) -> throw(found);
has_value(Val, {node, {_, _, Left, Right}}) ->
    has_value(Val, Left),
    has_value(Val, Right).
