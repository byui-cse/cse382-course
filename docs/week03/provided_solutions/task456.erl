-module(task456).
-export([sieve_primes/1,speed_test/0]).




c_filter(List,Predicate)->
	[X||X<-List, Predicate(X)==true].

sieve_primes(Numbers) ->
	c_filter(Numbers,fun(I)-> is_prime(I,[2,3,5,7]) end).

is_prime(I,Primes) ->
	Predicate = fun(P)-> (I /= P) and (I rem P == 0) end,
	not lists:any(Predicate,Primes).


speed_test() ->
	Numbers = lists:seq(2,20_000),


	statistics(runtime),
    statistics(wall_clock),

    %You only need to check fir divisibility for primes less than or equal to the square root of 20,000.
  	lists:filter(fun(I)-> is_prime(I,[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139]) end,Numbers),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

	
	statistics(runtime),
    statistics(wall_clock),

  	c_filter(Numbers,fun(I)-> is_prime(I,[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139]) end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

	io:format("speed test done~n").
	