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

-module(task456).
-export([sieve_primes/1,speed_test/0]).



%%
%% Generates a list based on an initial list and a predicate to apply 
%% to each element of the list. The predicate returns true if the item
%% is to be included in the resultant list. If not, the predicate 
%% returns false.
%%
%% Parameters - 1)List of elements of any kind
%%              2)Predicate used to evaluate the inclusion of each list element
%% Value - a list of elements for which the predicate had a value of true
%% Complexity - O(n)
%%
c_filter(List,Predicate)->
	[X||X<-List, Predicate(X)==true].

%% A function to exercise the c_filter function. the result is a list of prime numbers.
sieve_primes(Numbers) ->
	c_filter(Numbers,fun(I)-> is_prime(I,[2,3,5,7]) end).

%% A helper-function used as a predicate in the sieve_primes function.
is_prime(I,Primes) ->
	Predicate = fun(P)-> (I /= P) and (I rem P == 0) end,
	not lists:any(Predicate,Primes).

%% A function to evaluate the speed of c_filter relative to lists:filter.
%% To be even close to accurate, standard speed testing behaviors 
%% such as disconnecting from any network, closing all other applications,
%% turning off background processes, etc. would have to be done.
speed_test() ->
	Numbers = lists:seq(2,20_000),

	
	% Versions of the statistics function are used to time the execution of functions.
	statistics(runtime),
    statistics(wall_clock),

    %You only need to check for divisibility for primes less than or equal to the square root of 20,000.
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
	