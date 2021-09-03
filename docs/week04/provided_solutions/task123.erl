-module(task123).
-export([build_text/0,speed_test/0]).




c_foldl([],Accume,_)->Accume;
c_foldl([H|T],Accume,Fold_fun) ->
	c_foldl(T,Fold_fun(Accume,H),Fold_fun).


build_text() ->
	%I have chosen to use 2000 random Three Letter Acronyms (TLAs) as my words
	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),2000),
	%to do the concatenation, you probably would want to use iolist concatenation instead of regular string concatenation.
	c_foldl(TLAs,"",fun(Accume,C)-> C++" "++Accume end).

speed_test() ->


	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),2000),
	
	statistics(runtime),
    statistics(wall_clock),

  	lists:foldl(fun(C,Accume)-> C++" "++Accume end,"",TLAs),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    io:format("Standard foldr~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time1 * 1000,Time2 * 1000]),

	statistics(runtime),
    statistics(wall_clock),

  	c_foldl(TLAs,"",fun(Accume,C)-> C++" "++Accume end),

    {_, Time3} = statistics(runtime),
    {_, Time4} = statistics(wall_clock),
    io:format("Custom foldr~n\tCPU time: ~p, clock time:~p) microseconds~n",
    [Time3 * 1000,Time4 * 1000]),

	io:format("speed test done~n").


	