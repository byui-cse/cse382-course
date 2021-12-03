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
-module(trie).
-export([add/2,lookup/2]).


%%
%% @doc Used to add a list of elements to a trie. The trie is implemented as a dictionary 
%% where the key is an element and the value is another dictionary.
%%
%% Parameters - 1)A list of elements and
%%				2)A trie to which the elements are added. The trie may be empty.
%% Value - a trie.
%% Complexity - O(n log m) where n is the number of elements in the list and
%% m is the number of branches encountered when traversing the trie to do the addition.
%% 
add([],Trie)->
	add_remaining([],Trie);
add([H|T],Trie)->
	case dict:find(H,Trie) of
		{ok,Sub_trie}->
			dict:store(H,add(T,Sub_trie),Trie);
		_->
			dict:store(H,add_remaining(T,dict:new()),Trie)
	end.

%%
%% @doc Used to add a branch to a trie.
%%
%% Parameters - 1)A list of elements and
%%				2)A trie to which the elements are added.
%% Value - a trie.
%% Complexity - O(n)
%%
add_remaining([],Trie)->
	dict:store(word_end,none,Trie);
add_remaining([H|T],Trie)->
	dict:store(H,add_remaining(T,dict:new()),Trie).

%%
%% @doc Used to add a list of elements to a trie. The trie is implemented as a dictionary 
%% where the key is an element and the value is another dictionary.
%%
%% Parameters - 1)A list of elements and
%%				2)A trie that may contain the ordered elements of the list.
%% Value - {ok,found}, when the list is found, {fail,none} otherwise.
%% Complexity - O(n log m) where n is the number of elements in the list and
%% m is the number of branches encountered at each node during the search.
%% 
lookup([],Trie)->
	case dict:find(word_end,Trie) of
		{ok,none}->{ok,found};
		_->{fail,none}
	end;
lookup([H|T],Trie)->
	case dict:find(H,Trie) of
		error->{fail,none};
		{ok,Sub_trie}->lookup(T,Sub_trie)
	end.





