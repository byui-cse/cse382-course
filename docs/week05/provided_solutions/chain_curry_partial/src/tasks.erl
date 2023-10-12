
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
-export([chain/2,curry/1]).

%%
%% Executes a sequence of funs where, for each fun, the value of the first fun is the 
%% parameter of the next fun.
%%
%% Parameters - 1)List of funs each of which has one parameter
%%              2)The initial seed for the funs
%% Value - a single item or data collection of items
%% Complexity - The greater of O(n) and O(f(m)) where f(m) is the worst efficiency class of the set of funs being chained
%%
chain(Links,_) when is_list(Links) == false -> no_links;
chain([],_) -> no_links;
chain(_,[]) -> [];
chain(Links,Seed)->
	case lists:all(fun(Link)-> is_function(Link) end,Links) of
		true -> 
			lists:foldl(fun (Link,Value)-> Link(Value) end,Seed,Links);
		_ -> bad_links
	end.


curry(F) -> 
	{arity, Arity} = erlang:fun_info(F, arity),
	gather_and_execute(F,Arity,[]). 

%%%
%%% This is a helper function for curry. It is 'private' to this module.
%%%
%%% Hint: look at the apply functions. 
%%% https://erlang.org/documentation/doc-6.0/erts-6.0/doc/html/erlang.html#apply-2
%%%
gather_and_execute(F,0,Args) -> 
	apply(F,lists:reverse(Args)); 
gather_and_execute(F,Arity,Args) -> 
	fun (Arg) -> gather_and_execute(F,Arity - 1,Arg ++ Args) end.




%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
chain_test_()->
	Square_them = fun(List)-> [X*X||X<-List] end,
	Increment_them = fun(List)-> [X+3||X<-List] end,
	Multiply_them = fun(List)->lists:foldl(fun(X,Product)-> X*Product end,1,List) end,

	[?_assertEqual(1008,chain([Square_them,Increment_them,Multiply_them],[0,1,2,3])),%happy path
	 ?_assertEqual(129_600,chain([Increment_them,Square_them,Multiply_them],[0,1,2,3])),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],chain([Square_them,Increment_them,Multiply_them],[])),
	 ?_assertEqual(no_links,chain([],[0,1,2,3])),
	 ?_assertEqual(bad_links,chain([bob,sue,sally],[0,1,2,3])),
	 ?_assertEqual(no_links,chain(bob,[0,1,2,3])),
	 ?_assertEqual(no_links,chain(nil,[0,1,2,3])),
	 ?_assertEqual(no_links,chain(4,[0,1,2,3]))
	].

gather_and_execute_test_()->
	Do_math = fun(A,B,C,D)->(A+2)*B+(C div 3)+(10*D) end,
	No_params = fun()->ok end,
	[?_assertEqual(28,gather_and_execute(Do_math,0,[1,2,3,4])),%happy path
	 %nasty thoughts start here
	 ?_assertException(error,{badarity,{_,[a]}},gather_and_execute(No_params,0,[a])),
	 ?_assert(is_function(gather_and_execute(Do_math,[],4)))
	].
-endif.



	