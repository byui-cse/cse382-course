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

-module(task123).
-export([build_text/0,speed_test/0]).


%%
%% An implementation of the fold left functor pattern.
%%
%% Parameters - 1)A list of any type of elements that is to be folded into 1 value
%%              2)An initial value to which is merged the elements of the list
%%				3)A function used to merge the accumulator and each element of the list
%% Value - A single value of some type
%% Complexity - O(n*u) where u is the complexity of the fold-func function
%%
c_foldl([],Accume,_)->Accume;
c_foldl([H|T],Accume,Fold_fun) ->
	c_foldl(T,Fold_fun(Accume,H),Fold_fun).

%% An exercising function used to build a 
%% text string from a set of Three Letter Acronyms.
build_text() ->
	%I have chosen to use 2000 random Three Letter Acronyms (TLAs) as my words
	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),2000),
	%to do the concatenation, you probably would want to use iolist concatenation instead of regular string concatenation.
	c_foldl(TLAs,"",fun(Accume,C)-> C++" "++Accume end).

%%A testing function to display the speed of c_foldl relative to the native list:foldl function
speed_test() ->
	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),2000),
	
	statistics(runtime),
    statistics(wall_clock),

  	lists:foldl(fun(C,Accume)-> C++" "++Accume end,"",TLAs),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard foldl~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

	statistics(runtime),
    statistics(wall_clock),

  	c_foldl(TLAs,"",fun(Accume,C)-> C++" "++Accume end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom foldl~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

	io:format("speed test done~n").


	