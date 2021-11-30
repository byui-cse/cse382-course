%% @author Lee Barney
%% @copyright 2020 Lee Barney licensed under the <a>
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
-module(exercise).
-export([run/0]).


run()->
	Empty = {[],[]},
	First = {[a],[]},
	Last = {[],[b]},
	Both = {[a],[b]},
	io:format("Exercising empty function~n",[]),
	io:format("\t~p => ~p~n",[First,c_queue:empty(First)]),
	io:format("\t~p => ~p~n",[Last,c_queue:empty(Last)]),
	io:format("\t~p => ~p~n",[Both,c_queue:empty(Both)]),
	io:format("\t~p => ~p~n~n",[Empty,c_queue:empty(Empty)]),


	io:format("Exercising head function~n",[]),
	io:format("\t~p => ~p~n",[First,c_queue:head(First)]),
	io:format("\t~p => ~p~n",[Last,c_queue:head(Last)]),
	io:format("\t~p => ~p~n",[Both,c_queue:head(Both)]),
	io:format("\t~p => ~p~n~n",[Empty,c_queue:head(Empty)]),


	io:format("Exercising tail function~n",[]),
	io:format("\t~p => ~p~n",[First,c_queue:tail(First)]),
	io:format("\t~p => ~p~n",[Last,c_queue:tail(Last)]),
	io:format("\t~p => ~p~n",[Both,c_queue:tail(Both)]),
	io:format("\t~p => ~p~n~n",[Empty,c_queue:tail(Empty)]),


	io:format("Exercising enqueue function~n",[]),
	io:format("\t~p becomes ~p~n",[First,c_queue:enqueue(First,c)]),
	io:format("\t~p becomes ~p~n",[Last,c_queue:enqueue(Last,c)]),
	io:format("\t~p becomes ~p~n",[Both,c_queue:enqueue(Both,c)]),
	io:format("\t~p becomes ~p~n~n",[Empty,c_queue:enqueue(Empty,c)]),


	io:format("Exercising dequeue function~n",[]),
	io:format("\t~p becomes ~p~n",[First,c_queue:dequeue(First)]),
	io:format("\t~p becomes ~p~n",[Last,c_queue:dequeue(Last)]),
	io:format("\t~p becomes ~p~n",[Both,c_queue:dequeue(Both)]),
	io:format("\t~p becomes ~p~n",[Empty,c_queue:dequeue(Empty)]),
	io:format("\t~p becomes ~p~n",[c_queue:enqueue(Last,c),c_queue:dequeue(c_queue:enqueue(Last,c))]),
	io:format("\t~p becomes ~p~n~n",
		[c_queue:enqueue(c_queue:enqueue(Last,c),d),
		 c_queue:dequeue(c_queue:enqueue(c_queue:enqueue(Last,c),d))]),


	io:format("Exercising enqueue_front function~n",[]),
	io:format("\t~p becomes ~p~n",[First,c_queue:enqueue_front(First,c)]),
	io:format("\t~p becomes ~p~n",[Last,c_queue:enqueue_front(Last,c)]),
	io:format("\t~p becomes ~p~n",[Both,c_queue:enqueue_front(Both,c)]),
	io:format("\t~p becomes ~p~n~n",[Empty,c_queue:enqueue_front(Empty,c)]),


	io:format("Exercising dequeue_back function~n",[]),
	io:format("\t~p becomes ~p~n",[First,c_queue:dequeue_back(First)]),
	io:format("\t~p becomes ~p~n",[Last,c_queue:dequeue_back(Last)]),
	io:format("\t~p becomes ~p~n",[Both,c_queue:dequeue_back(Both)]),
	io:format("\t~p becomes ~p~n",[Empty,c_queue:dequeue_back(Empty)]),
	io:format("\t~p becomes ~p~n",
		[c_queue:enqueue(c_queue:enqueue(Last,c),d),
		 c_queue:dequeue_back(c_queue:enqueue(c_queue:enqueue(Last,c),d))]).

