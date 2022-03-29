%% @author Lee Barney
%% @copyright 2021 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
%% These exercises are not intended to be ideal examples of unit or other 
%% types of testing, nor are they intended to be ideal examples of how to 
%% exercise code. Instead, they are here to help you see how someone else 
%% might minimally exercise code they've written.
%%

-module(task7).
-export([c_unfold/3,test_behavior/0]).

%%
%% A naive implementation of the unfold functor pattern. This version is 
%%   very limited and should not be used in production.
%%
%% Parameters - 1)The number of items to be generated
%%              2)The current state of the unfold-process
%%				3)A lambda function to apply to the combination of 
%%                   the state and count that generates the next state and count
%% Value - A list of elements of the same type as the initial state
%% Complexity - O(n*u) where u is the complexity of the unfold-func
%%
c_unfold(0,_,_)->[];
c_unfold(Count,State,Unfold_fun) ->
	{A,B} = Unfold_fun(Count,State),
	[B]++c_unfold(A,B,Unfold_fun).


%% A function to begin exercising the c-unfold function
test_behavior() ->
	c_unfold(50,0,fun(C,S)-> {C-1,S+2} end) == [A*2 || A<-lists:seq(1,50)].


	