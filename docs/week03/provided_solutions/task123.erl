-module(task123).
-export([encipher/1,decipher/1,speed_test/0]).


%%
%% Generates a list based on an initial list and a predicate to apply 
%% to each element of the list.
%%
%% Parameters - 1)List of elements of any kind
%%              2)Predicate used to modify each list element
%% Value - a list of modified values
%% Complexity - O(n)
%%
c_map(List,Predicate)->
	[Predicate(X)||X <- List].

%% A silly function used to exercise the c_map function. 
%% It does a simple 'shift one' Caesar cipher.
encipher(String) ->
	c_map(String,fun(C)-> C+1 end).

%% A silly function used to exercise the c_map function.
%% It does a simple 'unshift one' to get the original text back.
decipher(String)->
	c_map(String,fun(C)-> C-1 end).

%% A function to evaluate the speed of c_map relative to lists:map.
%% To be even close to accurate, standard speed testing behaviors 
%% such as disconnecting from any network, closing all other applications,
%% turning off background processes, etc. would have to be done.
speed_test() ->
	Numbers = lists:seq(1,10_000_000),
	% Versions of the statistics function are used to time the execution of functions.

	statistics(runtime),
    statistics(wall_clock),

  	lists:map(fun(I) -> I+1 end,Numbers),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

	
	statistics(runtime),
    statistics(wall_clock),

  	c_map(Numbers,fun(I) -> I+1 end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),


	io:format("speed test done~n").
	