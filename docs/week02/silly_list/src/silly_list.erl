-module(silly_list).
-export([]).
%%%
%%% The code you will write and the tests that are already
%%% included in this module should NEVER be used in production.
%%% Your language already includes an optimized
%%% library of list BIFs. The only reason this module exists is
%%% to help you conceptualize how those BIFs might work and why
%%% some design choices are better than others.
%%%


%% The to_do atom indicates there is a coding task to do to 
%% complete the function.


%%%
%%%@doc
%%% This function creates a list with an additional element to the beginning of a list.
%%%
%%% Parameters: E - any valid Erlang term to be included in a list.
%%%             L - the list that is the basis for the addition
%%% 
%%% Value: A List with E found at the beginning of L.
%%%
%%% Time Complexity: Best Case - 𝛩(1)
%%% Space Complexity: Best Cast - 𝛩(1)
%%% @end
%%%
-spec prepend(term(),node())->node().
prepend(Element,List)->
    to_do.


%%%
%%%@doc
%%% This function creates a list with an additional element to the end of a list.
%%%
%%% Parameters: E - any valid Erlang term to be included in a list.
%%%             L - the list that is the basis for the addition
%%% 
%%% Value: A List with E found at the end of L.
%%%
%%% Time Complexity: Best Case - 𝛩(n)
%%% Space Complexity: Best Cast - 𝛩(n)
%%% @end
%%%
-spec append(term(),node())->node().
append(Element,List)->
    to_do.


%%%
%%%@doc
%%% This function removes the first element of the list.
%%%
%%% Parameters: L - the list that is the basis for removal
%%% 
%%% Value: A 2-tuple where the first element is what was the
%%% first element of the parameter L and the second parameter 
%%% is the everything in L except the first element. Oder is
%%% preserved in the second element.
%%%
%%% Time Complexity: 𝛩(1)
%%% Space Complexity: 𝛩(1)
%%% @end
%%%
-spec split_head(node())->{term(),node()}.
split_head(List)->
    to_do.


%%%
%%%@doc
%%% This function removes the last element of the list.
%%%
%%% Parameters: L - the list that is the basis for removal
%%% 
%%% Value: A 2-tuple where the first element is what was the
%%% last element of the parameter L and the second parameter 
%%% is the everything in L except the last element. Oder is
%%% preserved in the second element.
%%%
%%% Time Complexity: Best Case - 𝛩(1), Worst and Average Case O(n)
%%% Space Complexity: Best Cast - 𝛩(1), Worst and Average Case O(n)
%%% @end
%%%

-spec split_tail(node())->{term(),node()}.
split_tail(List)->
    to_do.

%%%
%%%@doc
%%% This function inerts an element infront of
%%% the location indicated.
%%% Parameters: E - the Element to insert
%%%             L - the one-based location of the element
%%%             Li - the list that is the basis for insertion
%%% 
%%% Value: A list with all of the elements of Li with E inserted.
%%%
%%% Time Complexity: Best Case - 𝛩(1), Worst and Average Case O(n)
%%% Space Complexity: Best Cast - 𝛩(1), Worst and Average Case O(n)
%%% @end
%%%

%This function follows the façade pattern.
-spec insert(term(),integer(),node())->node().
insert(E,L,Li)->
    insert(E,L,Li,1).

-spec insert(term(),integer(),node(),integer())->node().
insert(E,L,Li,Count)->
    todo.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
%%
%% This is where I have chosen to put the tests for this 
%% module. They could be moved into a separate file if 
%% that made more sense.
%%

prepend_test_()->
    [?_assertEqual({bob,{sally,{grace,{sue}}}}, prepend(bob,{sally,{grace,{sue}}})),%happy path
     %nasty paths start here
     ?_assertEqual({sue}, prepend(sue,nil)),%non-existant list
     ?_assertEqual({sue}, prepend(sue,{})) %empty list
    ].

append_test_()->
    [?_assertEqual({sally,{grace,{sue,{bob}}}}, append(bob,{sally,{grace,{sue}}})),%happy path
     %nasty paths start here
     ?_assertEqual({sue}, append(sue,nil)),%non-existant list
     ?_assertEqual({sue}, append(sue,{})) %empty list
    ].

split_head_test_()->
    [?_assertEqual({bob,{sally,{grace,{sue}}}}, split_head({bob,{sally,{grace,{sue}}}})),%happy path
     %nasty paths start here
     ?_assertEqual({fail,no_list}, split_head(nil)),%non-existant list
     ?_assertEqual({nil,{}}, split_head({})) %empty list
    ].

split_tail_test_()->
    [?_assertEqual({sue,{bob,{sally,{grace}}}}, split_head({bob,{sally,{grace,{sue}}}})),%happy path
     %nasty paths start here
     ?_assertEqual({fail,no_list}, split_head(nil)),%non-existant list
     ?_assertEqual({nil,{}}, split_tail({})) %empty list
    ].

insert_test_()->
    [?_assertEqual({sally,{bob,{grace,{sue}}}}, insert(bob,2,{sally,{grace,{sue}}})),%happy path
     %nasty paths start here
     ?_assertEqual({sally,{grace,{sue,{bob}}}}, insert(bob,4,{sally,{grace,{sue}}})),%at end
     ?_assertEqual({sally,{grace,{sue,{bob}}}}, insert(bob,10,{sally,{grace,{sue}}})),%at end
     ?_assertEqual({fail,no_list}, insert(bob,1,nil)),%non-existant list
     ?_assertEqual({bob}, insert(bob,1,{})), %empty list
     ?_assertEqual({bob}, insert(bob,10,{})), %empty list
     ?_assertEqual({sally,{grace,{sue,{bob}}}}, insert(bob,100,{sally,{grace,{sue}}}))
    ].



-endif.
