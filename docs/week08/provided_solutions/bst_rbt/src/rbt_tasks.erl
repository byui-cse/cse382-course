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
-module(rbt_tasks).
%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([add/3,contains/3]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Generate a Red-Black Tree (RBT) consisting of an initial tree with 
%%% the additional value included in the generated tree. A comparison
%%% fun with values of -1,less than, 0, equal, and 1, greater than,
%%% is used to determine placement
%%% of the value in the RBT. If nil is passed instead of a comparison
%%% fun, default Erlang comparisons are done.
%%%
%%% When necissary, the tree is rebalanced as part of the add function.
%%%
%%%
%%% Parameters - 1)A RBT 
%%%				 2)The value to add to the RBT
%%%				 3)A comparision function or nil. If nil, default Erlang
%%%					comparisions are done.
%%% Value - A RBT containing all the values of parameter 1 and the 
%%% parameter 2 value.
%%% Complexity - time: O(log n) space: O(log n)
%%% @end
%%%-------------------------------------------------------------------
-type rbt_node() :: {red | black,term(),rbt_node(),rbt_node()}.
-type rbt() :: rbt_node().
-spec add(rbt(),term(),fun((term(),term()) -> 1|0|-1) | nil()) -> rbt().

add(nil,To_add,_)->
	{black,To_add,nil,nil};
add(Root,To_add,Comparitor)->
	Comparison_function = case is_function(Comparitor) of 
		true -> Comparitor;
		_ -> fun(X,Y)-> bst_tasks:default_compare(X,Y) end
	end,
	{_,Value,Next_l,Next_r} = add_helper(Root,To_add,Comparison_function),
	{black,Value,Next_l,Next_r}.



%%%-------------------------------------------------------------------
%%% @doc
%%% Determine if a Red-Black Tree (RBT) contains a specified value. 
%%% A comparison fun/2 with values of  -1, less than, 0, equal, and 1, 
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

-spec contains(rbt(),term(),fun((term(),term()) -> 1 | 0 | -1) | nil()) -> boolean().
contains(nil,_,_)->
	false;
contains({_,Value,Next_l,Next_r},Search_value,Comparitor)->
	Comparison_function = case is_function(Comparitor) of 
		true -> Comparitor;
		_ -> fun(X,Y)-> default_compare(X,Y) end
	end,
	case Comparison_function(Search_value,Value) of
		0  ->
			true;
		-1 ->
			contains(Next_l,Search_value,Comparison_function);
		_  ->
			contains(Next_r,Search_value,Comparison_function)
	end.

%%
%% Helper functions
%%

%% a default comparison function for values.
-spec default_compare(term(),term())->1|0|-1.
default_compare(X,Y) when X < Y->  -1;
default_compare(X,Y) when X > Y-> 1;
default_compare(_,_)-> 0.


%% a worker function that does the computation needed by the add facade function
-spec add_helper(rbt(),term(),fun((term(),term()) -> 1|0|-1) | nil()) -> rbt().
add_helper(nil,To_add,_)->
	{red,To_add,nil,nil};
add_helper({Color,Value,Next_l,Next_r},To_add,Comparitor)->
	case Comparitor(To_add,Value) of
		-1 ->
			balance_left({Color,Value,add_helper(Next_l,To_add,Comparitor),Next_r});
		_  ->
			balance_right({Color,Value,Next_l,add_helper(Next_r,To_add,Comparitor)})
	end.

% balances the left branch of any RBT.

-spec balance_left(rbt())->rbt().
% black node has left red child with a left red grandchild
balance_left({black,Value,{red,A_value,{red,B_value,B_left,B_right},A_right},Next_r})->
	{red,A_value,{black,B_value,B_left,B_right},{black,Value,A_right,Next_r}};
% black node has left red child with a right red grandchild
balance_left({black,Value,{red,A_value,A_left,{red,B_value,B_left,B_right}},Next_r})->
	{red,B_value,{black,A_value,A_left,B_left},{black,Value,B_right,Next_r}};
%otherwise no balance needed
balance_left(Node)->
	Node.

% balances the right branch of any RBT.

-spec balance_right(rbt())->rbt().
% black node has right red child with a right red grandchild
balance_right({black,Value,Next_l,{red,A_value,A_left,{red,B_value,B_left,B_right}}})->
	{red,A_value,{black,Value,Next_l,A_left},{black,B_value,B_left,B_right}};
% black node has right red child with a left red grandchild
balance_right({black,Value,Next_l,{red,A_value,{red,B_value,B_left,B_right},A_right}})->
	{red,B_value,{black,Value,Next_l,B_left},{black,A_value,B_right,A_right}};
%otherwise no balance needed
balance_right(Node)->
	Node.



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

student_compare({Name_X,_,_},{Name_Y,_,_}) when Name_X < Name_Y -> -1;
student_compare({Name_X,_,_},{Name_Y,_,_}) when Name_X > Name_Y -> 1;
student_compare(_,_) -> 0.



contains_test_()->
	[?_assertEqual(true,contains({red,5,{black,3,nil,nil},{black,7,nil,nil}},5,nil)),%happy thoughts
	 ?_assertEqual(true,contains({red,5,{black,3,nil,nil},{black,7,nil,nil}},3,nil)),
	 ?_assertEqual(true,contains({red,5,{black,3,nil,nil},{black,7,nil,nil}},7,nil)),
	 ?_assertEqual(false,contains({red,5,{black,3,nil,nil},{black,7,nil,nil}},30,nil)),
	 %nasty thoughts
	 ?_assertEqual(false,contains(nil,30,nil))
	].

balance_left_test_()->
	[?_assertEqual({black,5,{red,3,nil,nil},nil},%happy thoughts
 	 	balance_left({black,5,{red,3,nil,nil},nil})),
 	 ?_assertEqual({red,5,{black,3,nil,nil},{black,7,nil,nil}},% the root being black is not part of balancing
 	 	balance_left({black,7,{red,5,{red,3,nil,nil},nil},nil})),
	 %Nasty thoughts
	 ?_assertEqual({black,5,nil,{red,7,nil,nil}},% shouldn't balance a right-branch of the tree
 	 	balance_left({black,5,nil,{red,7,nil,nil}})),
 	 ?_assertEqual({black,7,nil,{red,5,nil,{red,3,nil,nil}}},% shouldn't balance a right-branch of the tree
 	 	balance_left({black,7,nil,{red,5,nil,{red,3,nil,nil}}}))
 	 ].

balance_right_test_()->
	[?_assertEqual({black,5,nil,{red,3,nil,nil}},%happy thoughts
 	 	balance_right({black,5,nil,{red,3,nil,nil}})),
 	 ?_assertEqual({red,10,{black,7,nil,nil},{black,20,nil,nil}},% the root being black is not part of balancing
 	 	balance_right({black,7,nil,{red,10,nil,{red,20,nil,nil}}})),
	 %Nasty thoughts
	 ?_assertEqual({black,5,{red,3,nil,nil},nil},% shouldn't balance a left-branch of the tree
 	 	balance_right({black,5,{red,3,nil,nil},nil})),
 	 ?_assertEqual({black,7,{red,3,nil,{red,4,nil,nil},nil}},% shouldn't balance a left-branch of the tree
 	 	balance_right({black,7,{red,3,nil,{red,4,nil,nil},nil}}))
 	 ].
 
add_test_() ->
 	[?_assertEqual({black,7,nil,nil},add(nil,7,nil)),%happy thoughts
 	 ?_assertEqual({black,7,nil,{red,20,nil,nil}},add({black,7,nil,nil},20,nil)),
 	 ?_assertEqual({black,7,{red,3,nil,nil},nil},add({black,7,nil,nil},3,nil)),
 	 ?_assertEqual({black,7,{red,3,nil,nil},{red,20,nil,nil}},add({black,7,{red,3,nil,nil},nil},20,nil)),
 	 ?_assertEqual({black,20,{black,7,{red,3,nil,nil},nil},{black,100,nil,nil}},
 	 	add({black,7,{red,3,nil,nil},{red,20,nil,nil}},100,nil)),
 	 ?_assertEqual({black,20,{black,7,{red,3,nil,nil},nil},{black,100,{red,90,nil,nil},nil}},
 	 	add({black,20,{black,7,{red,3,nil,nil},nil},{black,100,nil,nil}},90,nil)),
 	 ?_assertEqual({black,20,{black,7,{red,3,nil,nil},nil},{red,90,{black,80,nil,nil},{black,100,nil,nil}}},
 	 	add({black,20,{black,7,{red,3,nil,nil},nil},{black,100,{red,90,nil,nil},nil}},80,nil)),
 	 %nasty thoughts
 	 ?_assertEqual({black,{sally,21,sophmore},
 	 					{red,{bob,18,freshman},nil,nil},
 	 					{red,{sue,22,senior},nil,nil}},
 	 	add({black,{sally,21,sophmore},nil,{red,{sue,22,senior},nil,nil}},{bob,18,freshman},fun(X,Y)-> student_compare(X,Y) end)),
 	 ?_assertEqual({black,{bob,18,freshman},
                        {black,{adam,19,freshman},nil,nil},
                        {black,{sally,21,sophmore},
                               nil,
                               {red,{sue,22,senior},nil,nil}}},
 	 	add({black,{sally,21,sophmore},{red,{bob,18,freshman},nil,nil},{red,{sue,22,senior},nil,nil}},{adam,19,freshman},fun(X,Y)-> student_compare(X,Y) end))
 	].
-endif.

