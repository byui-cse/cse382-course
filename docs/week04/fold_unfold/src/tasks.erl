
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
-export([c_foldl/3,c_foldr/3,c_unfold/3,generate_primes/1]).

%%
%% An implementation of the fold right functor pattern.
%%
%% Parameters - 1)A list of any type of elements that is to be folded into 1 value
%%              2)An initial value to which is merged the elements of the list
%%				3)A lambda function used to merge the accumulator and each element of the list
%% Value - A single value of some type
%% Complexity - O(n*u) where u is the complexity of the fold-func lambda function
%%
c_foldr(List,Accum,Fold_fun) ->
	to_do.

%%
%% An implementation of the fold left functor pattern.
%%
%% Parameters - 1)A list of any type of elements that is to be folded into 1 value
%%              2)An initial value to which is merged the elements of the list
%%				3)A lambda function used to merge the accumulator and each element of the list
%% Value - A single value of some type
%% Complexity - O(n*u) where u is the complexity of the fold-func lambda function
%%
c_foldl([H|T],Accume,Fold_fun) ->
	to_do.

%%
%% A naive implementation of the unfold functor pattern. This version is 
%%   very limited and should not be used in production.
%%
%% Parameters - 1)The number of items to be generated
%%              2)The current state of the unfold-process
%%				3)A lambda function to apply to the state that generates the next state and count
%% Value - A list of elements of the same type as the initial state
%% Complexity - O(n*u) where u is the complexity of the unfold-func lambda
%%
c_unfold(Count,State,Unfold_fun) ->
	to_do.



%%%-------------------------------------------------------------------
%%% @doc
%%% Generate a list of primes. All primes greater than 2 and
%%% 3 are of the form 6n plus or minus 1. An indicator, n, is 
%%% incremented from 1 to the specified limit. Each of these n's
%%% represents a value for 6n. Some of these values have no primes
%%% at plus or minus 1, others have one, and some have two.
%%%
%%%
%%% Parameters - 1)The number of values of n to check for primes.
%%% Value - A list prime numbers including 2 and 3.
%%% Complexity - O(n!)
%%% @end
%%%-------------------------------------------------------------------
generate_primes(Limit)->
	to_do.



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
c_unfold_test_()->
	[?_assertEqual([A*2 || A<-lists:seq(1,50)],c_unfold(50,0,fun(Count,State)-> {Count-1,State+2} end)),%happy path, generating even numbers
	 %nasty thoughts start here
	 ?_assertEqual(not_number,c_unfold(nil,bob,fun(X)-> X end)),
	 ?_assertEqual(not_number,c_unfold({hello,world},sue,fun(_X)-> true end))
	].

c_foldl_test_()->
	[?_assertEqual("hello there you are amazing ", c_foldl([hello,there,you,are,amazing],
															"",
															fun(Accum,Atom)-> Accum++atom_to_list(Atom)++" " end)),%happy path
	 ?_assertEqual(21,c_foldl([1,2,3,4,5,6],0,fun(Accum,X) -> Accum+X end)),%another happy path
	 %nasty thoughts start here
	 ?_assertMatch([],c_foldl([],[],fun(Accum,X)->Accum+X end)),
	 ?_assertEqual(not_list,c_foldl(hello,[],fun(Accum,X)->Accum+X end))
	].


c_foldr_test_()->
	[?_assertEqual("amazing are you there hello ", c_foldr([hello,there,you,are,amazing],
															"",
															fun(Accum,Atom)-> Accum++atom_to_list(Atom)++" " end)),%happy path
	 ?_assertEqual(21,c_foldr([1,2,3,4,5,6],0,fun(Accum,X) -> Accum+X end)),%another happy path
	 %nasty thoughts start here
	 ?_assertMatch([],c_foldr([],[],fun(Accum,X)->Accum+X end)),
	 ?_assertEqual(not_list,c_foldr(hello,[],fun(Accum,X)->Accum+X end))
	].


generate_primes_test_()->
	[?_assertEqual([13,11,7,5,3,2],generate_primes(2)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([61,59,53,47,43,41,37,31,29,23,19,17,13,11,7,5,3,2],
	 				generate_primes(10)),
	 ?_assertEqual([],generate_primes(0)),
	 ?_assertEqual(not_number,generate_primes(sue))
	].
-endif.



	