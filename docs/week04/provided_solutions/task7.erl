-module(task7).
-export([c_unfold/3,test_behavior/0]).




c_unfold(0,_,_)->[];
c_unfold(Count,State,Unfold_fun) ->
	{A,B} = Unfold_fun(Count,State),
	[B]++c_unfold(A,B,Unfold_fun).



test_behavior() ->
	c_unfold(50,0,fun(C,S)-> {C-1,S+2} end) == [A*2 || A<-lists:seq(1,50)].


	