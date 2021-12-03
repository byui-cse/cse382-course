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

%%
%% This monad has a specific set of meta-data it is tracking. Your situation may require
%% a different set. There will be some of the meta-data listed here you don't need. There 
%% will also be some meta-data you need to add. Examples of this could include things like;
%% 1) the elements removed,
%% 2) the elements added,
%% 3) the percentage of access requests for each element,
%% 4) the source of the elements added,
%% 5) the number of elements meeting some criteria, or
%% 6) anything else that meets the needs of your situation.
%% 

%%
%% This monad is incomplete. Other custom functions applicable to your specific use 
%% of random access lists should be added.
%%

%%
%% This monad follows the Maybe pattern. When a function is executed, 
%% there may be  a usable result or there may not be. The <kbd>ok</kbd> atom is used 
%% to indicate the existence of a usable value, <kbd>fail</kbd> is used to indicate 
%% an unusable value.
%%
%% As this monad only adds Maybe meta-data to the already existing ral module code, 
%% this monad can be written as a facade. If additional meta-data were needed for 
%% your situation, the RAL functions may need to be re-written to generate the 
%% needed meta-data.
%%

-module(ral_monad).
-export([id/1,bind/2,chain/2,get/1,update/1,cons/1]).

%%
%% @doc Used wrap a queue to be a monadal type.
%%
%% Parameters - 1)A RAL. 
%% Value - a tuple, {ok,RAL}, that has ok as the meta-data and the RAL as the data 
%% followed by the RAL as data.
%% Complexity - O(1)
%%
id(Ral)->{ok,Ral}.

%%
%% @doc Used to bind the functions of this monad together.
%% This function is rarely used independent of the chain function,
%% but is publicly available for unanticipated contingencies.
%%
%% Parameters - 1)A queue and its associated meta-data
%%				2)A function that is part of this monad 
%% Value - a tuple, {ok,queue} on success, {fail,_} on failure
%% Complexity - O(1) plus the complexity of the parameter that is the function
%%
bind(Qd={fail,_,_,_},_Func)-> Qd;
bind({_,First,Last,Length,Q},Func)->
	case Func(Q) of
		{fail,_,_,_,_} -> {fail,First,Last,Length,Q};

		{ok,F_next,L_next,Length_change,Q_next} -> 
			{ok,F_next,L_next,Length+Length_change,Q_next}
	end.

%%
%% @doc Used to execute a sequence of functions to be bound.
%%
%% Parameters - 1)A queue and its associated meta-data
%%				2)A list of functions that are part of this monad 
%% Value - a tuple, {nil,meta_data_and_queue} on success, {function,meta_data_and_queue} 
%% on failure
%% Complexity - O(1) plus the sum of the complexity of each function in the list
%%
chain(Mq,[])->Mq;
chain(Mq,[H|T])->
	case bind(Mq,H) of
		{fail,_,_,_,_}->{fail,Mq,H};
		Mq_result ->
			chain(Mq_result,T)
	end.


%%
%%A function used to retrieve a specified element from the RAL.
%%
%% Parameters: {Ral,Index} - a tuple consisting of the random access list 
%% containing the elements and the index of the desired element
%% Value: {ok_fail,element}the element at the specified index or none if there is no such index
%% Complexity: Best Case - O(1)
%% 			   Average Case - O(log n)
%%

get({Ral,Index})->
	case ral:get(Ral,Index) of
		none->{fail,none};
		Element->{ok,Element}.


%%
%%A function used to change a value stored at a specified index.
%%
%% Parameters: Ral - the source list for the change
%% 			   Index - the index of the element to be changed
%%			   Value - the new value for the given index.
%% Value: a tuple, {ok_fail,RAL}, containing a ok/failure indicator and
%% a list with the specified element changed and the others unchanged
%% Complexity: Best Case - O(1)
%%  		   Average Case - O(log n)
%%

update({Ral,Index,Value})
	{ok,ral:update(Ral,Index,Value,0)}.


%%A function to prepend an element to a random access list
%%
%% Parameters: Element - the element to prepend to the list
%% 			   Ral - the random access list to which the element is to be prepended
%% Value: a random access list with the specified element at index 0
%% Complexity: O(1)

cons({Element,Ral})->
	{ok,ral:cons(Element,Ral)}.
 cons(none,Ral)->Ral;




