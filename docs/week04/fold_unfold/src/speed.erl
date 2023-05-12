-module(speed).
-export([foldr_comparison/0,foldl_comparison/0]).

%%% A function to evaluate the speed of c_foldr relative to lists:foldr.
%%% To be even close to accurate, standard speed testing behaviors 
%%% such as disconnecting from any network, closing all other applications,
%%% turning off background processes, etc. would have to be done.
foldr_comparison() ->
    TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),2000),
    
    statistics(runtime),
    statistics(wall_clock),

    lists:foldr(fun(C,Accume)-> C++" "++Accume end,"",TLAs),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard foldr~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

    statistics(runtime),
    statistics(wall_clock),

    tasks:c_foldr(TLAs,"",fun(Accume,C)-> C++" "++Accume end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom foldr~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

    io:format("speed test done~n").

%%% A function to evaluate the speed of c_foldl relative to lists:foldl.
%%% To be even close to accurate, standard speed testing behaviors 
%%% such as disconnecting from any network, closing all other applications,
%%% turning off background processes, etc. would have to be done.
foldl_comparison() ->
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

    tasks:c_foldl(TLAs,"",fun(Accume,C)-> C++" "++Accume end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom foldl~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

    io:format("speed test done~n").

%%helper functions for the speed tests
shuffle(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.