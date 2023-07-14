%% @author Lee Barney
%% @copyright 2021 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are solutions that you can compare against yours to see
%% other options and to come up with even better solutions.
%%
-module(bst_tasks).
%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([add/3,contains/3]).


%%%-------------------------------------------------------------------
%%% @doc
%%% Generate a Binary Search Tree consisting of an initial tree with 
%%% the additional value included in the generated tree. A comparison
%%% fun with values of  -1, less than, 0, equal, and 1, greater than, 
%%% is used to determine placement
%%% of the value in the BST. If nil is passed instead of a comparison
%%% fun, default Erlang comparisons are done.
%%%
%%%
%%% Parameters - 1)A BST 
%%%				 2)The value to add to the BST
%%%				 3) A fun/2 used to compare values in the tree, or nil
%%% Value - An BST containing all the values of parameter 1 and the 
%%% parameter 2 value.
%%% Complexity - time: O(log n) space: O(log n)
%%% @end
%%%-------------------------------------------------------------------
-type bst_node() :: {term(),bst_node(),bst_node()}.
-type bst() :: bst_node().
-spec add(bst(),term(),fun((term(),term()) -> 1|0|-1) | nil()) -> bst().
add(BST,nil,_)->
	to_do.

%%%-------------------------------------------------------------------
%%% @doc
%%% Determine if a Binary Search Tree contains a specified value. 
%%% A comparison fun with values of  -1, less than, 0, equal, and 1, 
%%% greater than, is used to determine where to search
%%% next in the BST. If nil is passed instead of a comparison
%%% fun, default Erlang comparisons are done.
%%%
%%%
%%% Parameters - 1)A BST 
%%%				 2)The value to be searched for in the BST
%%%				 3) A fun/2 used to compare values in the tree, or nil
%%% Value - an atom, true when found, false when not
%%% Complexity - time: O(log n) space: O(1)
%%% @end
%%%-------------------------------------------------------------------

-spec contains(bst(),term(),fun((term(),term()) -> 1 | 0 | -1) | nil()) -> boolean().
contains(nil,_,_)->
	false;
contains({Value,Next_l,Next_r},Search_value,Comparitor)->
	to_do.

-spec default_compare(term(),term())->1|0|-1.
default_compare(X,Y) when X < Y-> 
	to_do.


%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
default_compare_test_()->
	[?_assertEqual(1,default_compare(10,2)),%happy path
	 ?_assertEqual(1,default_compare("hello","absolutely")),%happy path
	 ?_assertEqual(1,default_compare([1,2,3],[-1,2,3])),%happy path
	 ?_assertEqual(1,default_compare("hello","absolutely")),%happy path
	 ?_assertEqual(1,default_compare({10,sue},{10,bob})),%happy path
	 ?_assertEqual(0,default_compare("hello","hello")),%happy path
	 ?_assertEqual(0,default_compare([1,2,3],[1,2,3])),%happy path
	 ?_assertEqual(0,default_compare(50,50)),%happy path
	 ?_assertEqual(0,default_compare({10,sue},{10,sue})),%happy path
	 ?_assertEqual(-1,default_compare({6,sue},{10,sue})),%happy path
	 ?_assertEqual(-1,default_compare(6,10)),%happy path
	 ?_assertEqual(-1,default_compare("absolutely","hello")),%happy path
	 ?_assertEqual(-1,default_compare([-1,2,3],[1,2,3])),%happy path
	 %nasty thoughts start here
	 ?_assertEqual(1,default_compare({bob,sue},{alice,jane})),
	 ?_assertEqual(-1,default_compare(bob,[1,2,3])),
	 ?_assertEqual(1,default_compare(bob,-100))

	].
% comparison function used only in testing
 student_compare({_,Age_a,_},{_Name,Age_b,_}) when Age_a > Age_b -> 1;
 student_compare({_,Age_a,_},{_Name,Age_b,_}) when Age_a < Age_b -> -1;
 student_compare(_,_)  -> 0.

 add_test_() ->
 	[?_assertEqual({7,nil,nil},add(nil,7,nil)),%happy path,
 	 ?_assertEqual({25,nil,{50,nil,nil}},add({25,nil,nil},50,nil)),%happy path
 	 ?_assertEqual({25,{10,nil,nil},{50,nil,nil}},add({25,nil,{50,nil,nil}},10,nil)),%happy path
 	 ?_assertEqual({{bob,21,sophmore},nil,{{sue,22,senior},nil,nil}},
 	 				add({{bob,21,sophmore},nil,nil},{sue,22,senior},fun(X,Y)-> student_compare(X,Y) end)),%happy path
 	 ?_assertEqual({{bob,21,sophmore},{{sally,18,freshman},nil,nil},{{sue,22,senior},nil,nil}},
 	 				add({{bob,21,sophmore},nil,{{sue,22,senior},nil,nil}},{sally,18,freshman},fun(X,Y)-> student_compare(X,Y) end)),%happy path
 	 %nasty thoughts
 	 ?_assertEqual(nil,add(nil,nil,nil)),
 	 ?_assertEqual({7,nil,nil},add({7,nil,nil},nil,nil)),
 	 ?_assertEqual({7,nil,{10,nil,nil}},add({7,nil,nil},10,132)),%not a lambda
 	 ?_assertEqual({{bob,21,sophmore},nil,nil},add({{bob,21,sophmore},nil,nil},nil,fun(X,Y)-> student_compare(X,Y) end))

 	].
 contains_test_()->
 	[
 	 ?_assertEqual(true,contains({10,{8,nil,nil},{25,nil,nil}},10,nil)),%happy paths
 	 ?_assertEqual(true,contains({10,{8,nil,nil},{25,{12,nil,nil},nil}},12,nil)),
 	 ?_assertEqual(true,contains({10,{8,{4,nil,nil},{9,nil,nil}},{25,{12,nil,nil},nil}},4,nil)),
 	 ?_assertEqual(true,contains({10,{8,{4,nil,nil},{9,nil,nil}},{25,{12,nil,nil},nil}},9,nil)),
 	 ?_assertEqual(false,contains({10,{8,{4,nil,nil},{9,nil,nil}},{25,{12,nil,nil},nil}},100,nil)),
 	 ?_assertEqual(true,contains({{bob,21,sophmore},{{sally,18,freshman},nil,nil},{{sue,22,senior}}},
 	 	{sally,18,freshman},fun(X,Y)-> student_compare(X,Y) end)),
 	 %nasty thoughts
 	 ?_assertEqual(false,contains(nil,10,nil)),
 	 ?_assertEqual(true,contains({10,{8,nil,nil},{25,nil,nil}},10,not_a_fun)),
 	 ?_assertEqual(false,contains({10,{8,nil,nil},{25,nil,nil}},bob,not_a_fun))
 	].
-endif.

