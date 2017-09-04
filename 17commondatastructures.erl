-module('17commondatastructures').

-include("recordsheader.hrl").
-export([first_robot/0,
         car_factory/1,
         crusher_hobbies/0,
         nested/0,
         admin_panel/1,
         adult_section/1,
         repair_robot/1,
         included/0]).

-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).
-record(user, {id, name, group, age}).

first_robot() ->
    #robot{name="Mechatron",
           type=handmade,
           details=["moved by a small man inside"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.

crusher_hobbies() ->
    Crusher = #robot{name="Crusher",
                     hobbies=["crushin people", "petting cars"]},
    Crusher#robot.hobbies.

nested() ->
    NestedBot = #robot{details=#robot{name="erNest"}},
    NestedBot#robot.details#robot.name.





%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
    %% Show stuff that can't be written in such a text
    allowed;
adult_section(_) ->
    %% redirect to sesame street site
    forbidden.


%% The following function shows how to modify records.
repair_robot(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.


%% This function is using an included record:
included() ->
    #included{some_field="Some value"}.
