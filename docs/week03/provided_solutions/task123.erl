-module(task123).
-export([encipher/1,decipher/1,speed_test/0]).




c_map([],_)->[];
c_map([H|T],Mapping_fun) ->
	[Mapping_fun(H)]++c_map(T,Mapping_fun).


encipher(String) ->
	c_map(String,fun(C)-> C+1 end).

decipher(String)->
	c_map(String,fun(C)-> C-1 end).


speed_test() ->
	Numbers = lists:seq(1,10_000_000),
	
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
	