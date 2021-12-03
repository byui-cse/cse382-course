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

-module(exercise).
-export([run/0]).


run()->
	Initial = trie:add("Dani",dict:new()),
	io:format("found: ~p~n",[trie:lookup("Dani",Initial)]),
	io:format("found: ~p~n",[trie:lookup("Daniel",Initial)]),
	Three_names = trie:add("Daniel",trie:add("Daniella",Initial)),
	io:format("found: ~p~n",[trie:lookup("Dani",Three_names)]),
	io:format("found: ~p~n",[trie:lookup("Daniel",Three_names)]),
	io:format("found: ~p~n",[trie:lookup("Daniella",Three_names)]),
	Four_names = trie:add("Lee",Three_names),
	io:format("found: ~p~n",[trie:lookup("Dani",Four_names)]),
	io:format("found: ~p~n",[trie:lookup("Daniel",Four_names)]),
	io:format("found: ~p~n",[trie:lookup("Daniella",Four_names)]),
	io:format("found: ~p~n",[trie:lookup("Lee",Four_names)]),
	io:format("found: ~p~n",[trie:lookup("Sue",Four_names)]).

