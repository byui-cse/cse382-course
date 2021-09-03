-module(task456).
-export([build_text/0,speed_test/0]).




c_foldr([],Accume,_)->Accume;
c_foldr(List,Accume,Fold_fun) ->
	c_foldr(lists:droplast(List),Fold_fun(Accume,lists:last(List)),Fold_fun).


build_text() ->
	%I have chosen to use 200 random Three Letter Acronyms (TLAs) as my words
	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),200),
	%to do the concatenation, you probably would want to use iolist concatenation instead of regular string concatenation.
	c_foldr(TLAs,"",fun(Accume,C)-> C++" "++Accume end).

speed_test() ->
	TLAs = lists:sublist(list_lib:shuffle([[A,B,C]||A<-lists:seq(65,90),B<-lists:seq(65,90),C<-lists:seq(65,90)]),200),
	Start_standard = os:timestamp(),
  	Standard_text = lists:foldr(fun(C,Accume)-> C++" "++Accume end,"",TLAs),
  	io:format("standard foldr function total time taken ~f seconds~n~n", [timer:now_diff(os:timestamp(), Start_standard) / 1_000_000]),

	Start_custom = os:timestamp(),
  	Custom_text = c_foldr(TLAs,"",fun(Accume,C)-> C++" "++Accume end),
  	io:format("custom foldr function total time taken ~f seconds~n~n", [timer:now_diff(os:timestamp(), Start_custom) / 1_000_000]),

  	%io:format("TLA's: ~p~n",[TLAs]),
  	%io:format("standard: ~p~n",[Standard_text]),
  	%io:format("custom: ~p~n",[Custom_text]),
  	io:format("texts are same: ~p~n",[Standard_text == Custom_text]),
	io:format("speed test done~n").


	