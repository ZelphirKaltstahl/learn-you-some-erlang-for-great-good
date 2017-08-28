-module('09tree').
-export([empty/0,
         insert/3,
         lookup/2]).

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
