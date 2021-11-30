%% @author Lee Barney
%% @copyright 2020 Lee Barney licensed under the <a>
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
-module(queue_monad).
-module(c_queue).
-export([id/1,bind/2,chain/2,enqueue/2,dequeue/1,enqueue_front/2,dequeue_back/1]).

%%
%% This monad follows the Maybe pattern. When a function is executed, there may be 
%% a usable result or there may not be. The <kbd>ok</kbd> atom is used to indicate 
%% the existence of a usable value, <kbd>fail</kbd> is used to indicate an unusable
%% value.
%%

%% This monad is incomplete. Other custom functions applicable to your specific use 
%% of queues should be added.


%%
%% @doc Used wrap a queue to be a monadal type.
%%
%% Parameters - 1)A queue. 
%% Value - a tuple, {ok,first,last,queue}, that has as meta-data, ok, the first 
%% element in the queue, and the last element in the queue, followed by the queue as data.
%% Complexity - O(1)
%%
id(Q={F,R})->{ok,head(Q),tail(Q),length(F)+length(R),Q}.

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
bind(Qd={fail,_,_,_},Func)-> Qd;
bind(Qd={_,First,Last,Length,Q},Func})->
	case apply(Func,[Q]) of
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
%% @doc Peeks at the element that is the head of the queue. Not a publicly available function.
%%
%% Parameters - 1)A queue
%% Value - the first element of the queue
%% Complexity - Average case: O(1). Worst case O(n)
%%
head(Q={[],[]})->nil
head(Q={[],R})->lists:last(R);
head(Q={[H|_],_})->H.

%%
%% @doc Peeks at the element that is the end of the queue. Not a publicly available function.
%%
%% Parameters - 1)A queue
%% Value - the last element or nil as meta-data
%% Complexity - Average case: O(1). Worst case O(n).
%%
tail(Q={[],[]})->nil;
tail(Q={F,[]})->lists:last(F);
tail(Q={_,[H|_]})->H.

%%
%% @doc adds an element to the end of the queue.
%%
%% Parameters - 1)A tuple consisting of queue and an element to add {Q,E}
%% Value - a tuple with meta-data consisting of ok, the length of the tuple, the head of the tuple, 
%% the tail of the tuple, and the data being a queue with both the existing and additional elements.
%% Complexity - O(1).
%%
enqueue({Q={[],R=[]},A})->{ok,A,A,1,{[A],R}};
enqueue({Q={F,R},A})->{ok,head(F),A,1,{F,[A]++R}}.

%%
%% @doc adds a sequence of elements to the end of the queue.
%%
%% Parameters - 1)A tuple consisting of queue and a list of elements to add {Q,Elements}
%% Value - a tuple with meta-data consisting of ok, the length of the tuple, the head of the tuple, 
%% the tail of the tuple, the number of elements added, and the data being a queue with both the existing and additional elements.
%% Complexity - O(n), n being the number of elements being added.
%%
enqueue_many({{F,R},Elements})->
	Next_r = Elements++R,
	{ok,head(F),lists:last(Elements),length(Elements),{F,Next_r}}.

%%
%% @doc removes the element at the beginning of the queue.
%%
%% Parameters - 1)A queue
%% Value - a queue with the head element removed
%% Complexity - Average case:O(1). Worst case: O(n).
%%
dequeue(Q={[],[]})->{fail,nil,nil,0,Q};
dequeue(Q={[],R})->
	[_|T]=lists:reverse(R),
	{ok,head(T),tail(T),-1,{T,[]}};
dequeue(Q={[_|[]],R={Rh|Rt}})->
	[Uh|Ut] = Updated = lists:reverse(R),
	%[Uh|Ut] = Updated,
	{ok,Uh,Rh,-1,{Updated,[]}};
dequeue(Q={[_|T],R})->
	[H|_]=T,
	[Rh|_]=R,
	{ok,H,Rh,-1,{T,R}}.

%%
%% @doc prepends an element to the queue.
%%
%% Parameters - 1)A queue
%%				2)An element
%% Value - a queue with both the old and new elements
%% Complexity - O(1).
%%
enqueue_front({F,R=[H|T]},A)->{ok,A,H,1,{[A]++F,R}}.

%%
%% @doc removes the element at the end the queue.
%%
%% Parameters - 1)A queue
%% Value - a queue with the last element removed
%% Complexity - Average case: O(1). Worst case: O(n)
%%
dequeue_back(Q={[],[]})->{fail,nil,nil,0,Q};
dequeue_back({F=[H|T],[]})->
	[_Last|Remaining] = lists:reverse(F),
	{ok,H,lists:first(Remaining),-1,lists:reverse(Remaining)};
dequeue_back({[H|_],[_|T=[End|_]]})-> {ok,H,End,-1,T}.



