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
-module(task1).
-export([chain/2, speed_test/0]).

%%
%% Executes a sequence of funs where, for each fun, the value of the first fun is the 
%% parameter of the next fun.
%%
%% Parameters - 1)List of funs each of which has one parameter
%%              2)The initial seed for the funs
%% Value - a single item or data collection of items
%% Complexity - O(n*f) where f is the product of the complexities of each fun in the list
%%
chain(List,Item)->%this function is a facade
	chain_worker(lists:reverse(List),Item).

%% this function performs the actual chaining.
chain_worker([],Item)->
	Item;
chain_worker([H|T],Item)->
	H(chain_worker(T,Item)).



speed_test()->
	%Generate a list of funs that don't really do much.
	Chain_links = [fun(_) -> X end || X <-  lists:seq(1, 1_000_000)],
	
	%Use the statistics BIF to time the running of all the funs by fold
	io:format("Testing foldl chaining.~n"),
	statistics(runtime),
    statistics(wall_clock),

    lists:foldl(fun (Link,Value)-> Link(Value) end,1,Chain_links),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000,
    U2 = Time2 * 1000,
    io:format("\tCPU time: ~p, clock time:~p) microseconds~n",
    [U1,U2]),

    %Use the statistics BIF to time the running of all the funs by fold.
    %To get the funs to run in the correct order, the list must be reversed.
    io:format("Testing foldr chaining.~n"),
	statistics(runtime),
    statistics(wall_clock),
    Reversed = lists:reverse(Chain_links),
    lists:foldr(fun (Link,Value)-> Link(Value) end,1,Reversed),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    U3 = Time3 * 1000,
    U4 = Time4 * 1000,
    io:format("\tCPU time: ~p, clock time:~p) microseconds~n",
    [U3,U4]),

	%Use the statistics BIF to time the running of all the funs by the custom chain function
    io:format("Testing chain function chaining.~n"),
	statistics(runtime),
    statistics(wall_clock),

    chain(Chain_links,1),

    {_, Time5} = statistics(runtime),
    {_, Time6} = statistics(wall_clock),
    U5 = Time5 * 1000,
    U6 = Time6 * 1000,
    io:format("\tCPU time: ~p, clock time:~p) microseconds~n",
    [U5,U6]).
