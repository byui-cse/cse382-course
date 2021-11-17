-module(rbt).
-export([add/3,add_helper/3,contains/3,exercise/2]).



add(null,To_add,_)->
	{black,To_add,null,null};
add(Root,To_add,Comparitor)->
	{_,Value,Next_l,Next_r} = add_helper(Root,To_add,Comparitor),
	{black,Value,Next_l,Next_r}.

add_helper(null,To_add,_)->
	{red,To_add,null,null};
add_helper({Color,Value,Next_l,Next_r},To_add,Comparitor)->
	case Comparitor(To_add,Value) of
		-1 ->
			balance_left({Color,Value,add_helper(Next_l,To_add,Comparitor),Next_r});
		_  ->
			balance_right({Color,Value,Next_l,add_helper(Next_r,To_add,Comparitor)})
	end.


contains(null,_,_)->
	false;
contains({_,Value,Next_l,Next_r},Search_value,Comparitor)->
	case Comparitor(Search_value,Value) of
		0  ->
			true;
		-1 ->
			contains(Next_l,Search_value,Comparitor);
		_  ->
			contains(Next_r,Search_value,Comparitor)
	end.



% should always end up with {red,y,{black,x,a,b},{black,z,c,d}} for values
balance_left({black,Value,{red,A_value,{red,B_value,B_left,B_right},A_right},Next_r})->
	{red,A_value,{black,B_value,B_left,B_right},{black,Value,A_right,Next_r}};

balance_left({black,Value,{red,A_value,A_left,{red,B_value,B_left,B_right}},Next_r})->
	{red,B_value,{black,A_value,A_left,B_left},{black,Value,B_right,Next_r}};
balance_left(Node)->
	Node.

balance_right({black,Value,Next_l,{red,A_value,A_left,{red,B_value,B_left,B_right}}})->
	{red,A_value,{black,Value,Next_l,A_left},{black,B_value,B_left,B_right}};
balance_right({black,Value,Next_l,{red,A_value,{red,B_value,B_left,B_right},A_right}})->
	{red,B_value,{black,Value,Next_l,B_left},{black,A_value,B_right,A_right}};
balance_right(Node)->
	Node.


compare(X,Y) when X < Y-> -1;
compare(X,Y) when X > Y-> 1;
compare(_,_)-> 0.


exercise(Upper_limit,Count)->
	%Set the the random number generator's seed to a specific value.
	_ = rand:seed(default,{123, 123534, 345345}),
	Numbers=[rand:uniform(Upper_limit)||_<-lists:seq(1,Count)],
	%Exercise add
	Tree = lists:foldl(fun(Num,Accum_root)-> 
					add(Accum_root,Num,fun(X,Y)-> compare(X,Y) end) 
				end,null,Numbers),
	io:format("~p~n",[Tree]),
	%Exercise contains
	io:format("Contains Yes: ~n~p~n",[[contains(Tree,Num,fun(X,Y)-> compare(X,Y) end)||Num<-Numbers]]),
	%Non existent numbers
	io:format("Contains No: ~p~n",[[contains(Tree,-Num,fun(X,Y)-> compare(X,Y) end)||Num<-Numbers]]).

