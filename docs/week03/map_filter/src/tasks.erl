
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
%%% Created : 8 May 2023 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------

-module(tasks).

%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([c_map/2,c_filter/2,pick_candidates/1]).


%%%-------------------------------------------------------------------
%%% @doc
%%% Generates a list based on an initial list and a predicate to apply 
%%% to each element of the list.
%%%
%%% Parameters - 1)List of elements of any kind
%%%              2)Predicate used to modify each list element
%%% Value - a list of modified values
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
c_map(List,Predicate)->
	to_do.


%%%-------------------------------------------------------------------
%%% @doc
%%% Generates a list based on an initial list and a predicate to apply 
%%% to each element of the list. The predicate returns true if the item
%%% is to be included in the resultant list. If not, the predicate 
%%% returns false.
%%%
%%% Parameters - 1)List of elements of any kind
%%%              2)Predicate used to evaluate the inclusion of each list element
%%% Value - a list of elements for which the predicate had a value of true
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
c_filter(List,Predicate)->
	to_do.


%%%-------------------------------------------------------------------
%%% @doc
%%% From a list of job applicants, this function selects those with Erlang 
%%% or JavaScript experience and assigns them either to be backend (Erlang
%%% or Erlang and JavaScript) or frontend (JavaScript only) job candidates. 
%%%
%%% Ordering of the candidates is preserved.
%%%
%%% Parameters - 1)List of applicants. Each applicant is a 3-tuple described 
%%%  				by {name, years_experience,[skill]} where [skill] is the 
%%%					candidates list of skills.
%%% Value - A list of candidates. Each candidate is a 4-tuple described
%%%					by {name, assigned_to,years_experience,[skill]} where
%%%					assigned_to can be either frontend or backend.
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
pick_candidates(Applicants)->
	to_do.


%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

c_map_test_()->
	[?_assertEqual([2,4,6,10],c_map([1,2,3,5],fun(X)-> X*2 end)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],c_map([],fun(X)->X end)),
	 ?_assertEqual(no_list_error,c_map(nil,fun(X)-> X end)),
	 ?_assertEqual(no_list_error,c_map({hello,world},fun(_X)-> true end))
	].
c_filter_test_()->
	[?_assertEqual([2,4],c_filter([1,2,3,4,5],fun(X)-> (X rem 2) == 0 end)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],c_filter([],fun(_X)-> true end)),
	 ?_assertEqual(no_list_error,c_filter(nil,fun(_X)-> true end)),
	 ?_assertEqual(no_list_error,c_filter({hello,world},fun(_X)-> true end))
	].
pick_candidates_test_()->
	[?_assertEqual([{sue,2,backend,[erlang,c,rust]}],
					 pick_candidates([{joe,1,[c,r,swift]},
									  {sue,2,[erlang,c,rust]},
									  {sally,1,[kotlin]}])),%happy path
	 ?_assertEqual([{joe,1,frontend,[zap,javascript]}],
	 				pick_candidates([{joe,1,[zap,javascript]},
									  {sue,2,[c,rust]},
									  {sally,1,[kotlin]}])),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],pick_candidates([{joe,1,[zap,cobol]},
									  {sue,2,[c,rust]},
									  {sally,1,[kotlin]}])),
	 ?_assertEqual([],pick_candidates([])),
	 ?_assertEqual(no_list_error,pick_candidates(hello)),
	 ?_assertEqual(no_list_error,pick_candidates({hello,world}))
	].

-endif.



	