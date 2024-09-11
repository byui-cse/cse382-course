-module(custom_lists).
-export([add/2,pop/1,first/1,contains/2]).
%%%
%%% The code you will write and the tests that are already
%%% included in this module should NEVER be used in production.
%%% Erlang already includes an optimized
%%% library of list BIFs. The only reason this module exists is
%%% to help you conceptualize how those BIFs might work and why
%%% some design choices are better than others.
%%%


%% The to_do atom indicates there is a coding task to do to 
%% complete the function.

%% put add documentation here
-spec add(term(),node())->node().


%% put pop documentation here
-spec pop(term(),node())->node().


%% put first documentation here
-spec first(node())->term().


%% put contains documentation here
-spec contains(term(),node())->boolean().


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
%%
%% This is where I have chosen to put the tests for this 
%% module. They could be moved into a separate file if 
%% that made more sense.
%%

add_test_()->
	[?_assertEqual({bob,{sally,{grace,{sue,nil}}}}, add(bob,{sally,{grace,{sue,nil}}})),%happy path
     %nasty paths start here
     ?_assertEqual({sue,nil}, add(sue,nil)),%non-existant list
     ?_assertEqual({sue,nil}, add(sue,{})) %empty list
    ].

pop_test_()->
	[?_assertEqual({grace,{sue,nil}}, pop({sally,{grace,{sue,nil}}})),%happy path
     %nasty paths start here
     ?_assertEqual(nil, pop(nil)),%non-existant list
     ?_assertEqual({}, pop({})) %empty list
    ].

first_test_()->
	[?_assertEqual(bob, first({bob,{sally,{grace,{sue,nil}}}})),%happy path
     %nasty paths start here
     ?_assertEqual({fail,no_list}, first(nil)),%non-existant list
     ?_assertEqual({fail,empty_list}, first({})) %empty list
    ].

contains_test_()->
	[?_assertEqual(true, contains(grace,{bob,{sally,{grace,{sue}}}})),%happy path
     %nasty paths start here
     ?_assertEqual(false, contains(gertrude,{bob,{sally,{grace,{sue}}}})),
     ?_assertEqual(false, contains(gertrude,nil)),%non-existant list
     ?_assertEqual(false, contains(gertrude,{})) %empty list
    ].

-endif.
