
%%%-------------------------------------------------------------------
%%% @author Lee Barney
%%% @copyright 2023 Lee Barney licensed under the <a>
%%%        rel="license"
%%%        href="http://creativecommons.org/licenses/by/4.0/"
%%%        target="_blank">
%%%        Creative Commons Attribution 4.0 International License</a>
%%%
%%% Project - CSE 382
%%%
%%% @doc
%%% These solutions are not intended to be ideal solutions. Instead,
%%% they are solutions that you can compare against yours to see
%%% other options and to come up with even better solutions.
%%%
%%% You can find the unit tests to be passed near the bottom of this 
%%% file, just before the speed tests.
%%% @end
%%% Created : 10 May 2023 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------

-module(tasks).

%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([append/1,merge/1,difference/1,id/0,maybe_id/1,maybe_bind/2,monad_chain/3]).

%%
%% Appends one list to another.
%%
%% Parameter - a 2-tuple consisting of two lists
%% Value - a 2-tuple consisting of two lists where the first list is the first
%% element of the parameter tuple followed by the second element of the 
%% parameter tuple. The second element of the value tuple is an empty list.
%% Complexity - O(1)
%%

append({List_a,List_b})->
	to_do.

%%
%% Finds elements of a list that are not in another list.
%%
%% Parameter - a 2-tuple consisting of two lists
%% Value - a 2-tuple consisting of two lists where the first list's
%% elements are those elements of the parameter tuple's first list not
%% found in the parameter tuple's second list. The second element of the
%% value tuple is an empty list.
%% Complexity - O(n)
%%

difference({List_a,List_b})->
	to_do.

%%
%% Intermingles two lists by selecting, one by one, an element from each list.
%%
%% Parameter - a 2-tuple consisting of two lists
%% Value - a 2-tuple consisting of two lists where the first list consists
%% of all the values from both lists. The second element of the
%% value tuple is an empty list.
%% Complexity - O(n)
%%
merge({List_a,List_b})->
	to_do.


%%
%% Produces an identity element for the monoid consisting of merge, difference, and
%% append.
%%
%% Parameter - none
%% Value - a 2-tuple consisting of two empty lists.
%% Complexity - O(1)
%%

id()->to_do.

%%
%% Produces an identity element for a monad consisting of this identity and the  
%% monad_chain, maybe_bind, from_user, get, and save monadal functions.
%%
%% Parameter - the data to be wrapped in a monadal type
%% Value - a 2-tuple consisting of ok, and the data parameter
%% Complexity - O(1)
%%
maybe_id(Data)-> to_do.

%%
%% Unwraps the monadal type and applies the lambda function to the value in the monadal type.
%%
%% Parameters - 1) a monadal type 2-tuple who's first element is either ok or fail. The second
%% element of the monadal type is the data used by the monadal functions. The second parameter
%% is the lambda function to bind.
%% Value - On success, a 2-tuple monadal type consisting of ok for the first element and the 
%% result of applying the lambda to the data parameter. On failure, a 3-tuple monadal type
%% consisting of fail, the lambda that failed to correctly work with the data parameter, 
%% and the data that caused the failure. 
%% Complexity - O(f(n)) where f(n) is the complexity of the lambda function.
%%
maybe_bind({fail,_Failed_lambda,_Data} = Monadal_type,_Lambda)->Monadal_type;
maybe_bind({_,Data},Lambda)->
	to_do.

%%
%% Applies the list of data manipulation lambda functions, in the order declared, to a 
%% monadal type.
%%
%% Parameters - 1) a monadal type.
%%			  - 2) a binding function that is part of the monad being used
%%			  - 3) a list of data minipulation monadal lambda functions
%% Value - a second monadal type as defined by the monadal lambda functions applied. 
%% Complexity - O(f(n)) where f(n) is the greates complexity of the monadal lambda functions.
%%
monad_chain(Monadal_type,Binding_lambda,Data_manipulators)->
	to_do.




%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% tests for the monoid functions.
id_test()->
	?assertEqual({[],[]},id()).

append_test_()->
	[?_assertEqual({[a,b,c,d,e],[]},append({[a,b,c],[d,e]})),%happy path
	 %nasty thoughts start here
	 ?_assertEqual({[],[]},append({[],[]})),
	 ?_assertEqual({[a],[]},append({[a],[]})),
	 ?_assertEqual({[b],[]},append({[],[b]}))
	].

difference_test_()->
	[?_assertEqual({[a,b,c],[]},difference({[a,b,c,d,e],[d,e]})),%happy path
	 %nasty thoughts start here
	 ?_assertEqual({[],[]},difference({[],[]})),
	 ?_assertEqual({[a],[]},difference({[a],[]})),
	 ?_assertEqual({[],[]},difference({[],[a]})),
	 ?_assertEqual({[],[]},difference({[a,b,c],[a,b,c]})),
	 ?_assertEqual({[a,b,c],[]},difference({[a,b,c],[d,e]}))
	 ].

%% tests for the monad
maybe_id_test()->
	?assertEqual({ok,[bob,sue,gertrude]},maybe_id([bob,sue,gertrude])).

maybe_bind_test_()->
	[?_assertEqual({ok,[1,2,3]},maybe_bind({ok,[1,2,3]},fun(Data)-> Data end)),%happy path
	 ?_assertEqual({ok,6},maybe_bind({ok,[1,2,3]},fun(Data)-> lists:foldl(fun(X,Accum)-> X + Accum end,0,Data) end)),%happy path
	 %nasty thoughts start here
	 ?_assertMatch({fail,_Lambda,[1,2,3]},maybe_bind({ok,[1,2,3]},fun(_Data)-> maybe_failure end))
	 ].


monad_chain_test_()->
	Data_modification_manipulators = [fun(Data) -> lists:map(fun(X)->X*X end,Data) end,
									  fun(Data) -> lists:map(fun(X)->X+X end,Data) end,
									  fun(Data) -> lists:foldl(fun(X,Accum)->Accum+X end,0,Data) end],
	[?_assertEqual({ok,28},monad_chain({ok,[1,2,3]},
											fun(Data,Lambda)-> maybe_bind(Data,Lambda) end,
											Data_modification_manipulators)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual({ok,[1,2,3]},monad_chain({ok,[1,2,3]},
											fun(Data,Lambda)-> maybe_bind(Data,Lambda) end,
											[]))
	 ].

-endif.



	