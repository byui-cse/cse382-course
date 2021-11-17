-module(bst).
-export([add/3,contains/3,exercise/2]).



add(null,To_add,_)->
	{To_add,null,null};
add({Value,Next_l,Next_r},To_add,Comparitor)->
	case Comparitor(To_add,Value) of
		-1 ->
			{Value,add(Next_l,To_add,Comparitor),Next_r};
		_  ->
			{Value,Next_l,add(Next_r,To_add,Comparitor)}
	end.


contains(null,_,_)->
	false;
contains({Value,Next_l,Next_r},Search_value,Comparitor)->
	case Comparitor(Search_value,Value) of
		0  ->
			true;
		-1 ->
			contains(Next_l,Search_value,Comparitor);
		_  ->
			contains(Next_r,Search_value,Comparitor)
	end.


compare(X,Y) when X < Y-> -1;
compare(X,Y) when X > Y-> 1;
compare(_,_)-> 0.


exercise(Upper_limit,Count)->
	%Set the the random number generator's seed to a specific value.
	_ = rand:seed(default,{123, 123534, 345345}),
	Numbers=[rand:uniform(Upper_limit)||_<-lists:seq(1,Count)],
	Root = {3,null,null},
	%Exercise add
	Left_list = lists:foldl(fun(Num,Accum_root)-> 
					add(Accum_root,Num,fun(X,Y)-> compare(X,Y) end) 
				end,Root,Numbers),
	io:format("~p~n",[Left_list]),
	%Exercise contains
	[contains(Left_list,Num,fun(X,Y)-> compare(X,Y) end)||Num<-Numbers],
	%Non existent numbers
	[contains(Left_list,Num,fun(X,Y)-> compare(X,Y) end)||Num<-Numbers].

