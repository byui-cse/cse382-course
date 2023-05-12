-module(speed).
-export([filter_test/0,map_test/0]).

%%% A function to evaluate the speed of c_map relative to lists:map.
%%% To be even close to accurate, standard speed testing behaviors 
%%% such as disconnecting from any network, closing all other applications,
%%% turning off background processes, etc. would have to be done.
map_test() ->
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

  	tasks:c_map(Numbers,fun(I) -> I+1 end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),


	io:format("map speed test done~n").

%%% A function to evaluate the speed of c_filter relative to lists:filter.
%%% To be even close to accurate, standard speed testing behaviors 
%%% such as disconnecting from any network, closing all other applications,
%%% turning off background processes, etc. would have to be done.
filter_test() ->
	Numbers = lists:seq(1,10_000_000),

	
	% Versions of the statistics function are used to time the execution of functions.
	statistics(runtime),
    statistics(wall_clock),

    %keep only the even numbers
    lists:filter(fun(I)-> I rem 2 == 0 end,Numbers),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

	
	statistics(runtime),
    statistics(wall_clock),

  	tasks:c_filter(Numbers,fun(I)-> I rem 2 == 0 end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

	io:format("filter speed test done~n").