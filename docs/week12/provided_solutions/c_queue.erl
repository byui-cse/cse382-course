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
-module(c_queue).
-export([empty/1,head/1,tail/1,enqueue/2,dequeue/1,enqueue_front/2,dequeue_back/1]).

%%
%% @doc Used to determine if the queue has or does not have elements.
%%
%% Parameters - 1)A queue 
%% Value - true if the queue has no elements, false otherwise
%% Complexity - O(1)
%%
empty({[],[]})->true;
empty({_F,_R})->false.

%%
%% @doc Peeks at the element that is the head of the queue.
%%
%% Parameters - 1)A queue
%% Value - the first element, nil otherwise
%% Complexity - Average case: O(1). Worst case O(n)
%%
head({[],[]})->nil;
head({[],R})->lists:last(R);
head({[H|_],_})-> H.

%%
%% @doc Peeks at the element that is the end of the queue.
%%
%% Parameters - 1)A queue
%% Value - the last element, nil otherwise
%% Complexity - Average case: O(1). Worst case O(n).
%%
tail({[],[]})->nil;
tail({F,[]})->lists:last(F);
tail({_,[H|_]})->H.

%%
%% @doc adds an element to the end of the queue.
%%
%% Parameters - 1)A queue
%% 				2)An element
%% Value - a queue with both the old and new elements
%% Complexity - O(1).
%%
enqueue({[],R=[]},A)->{[A],R};
enqueue({F,R},A)->{F,[A]++R}.

%%
%% @doc removes the element at the beginning of the queue.
%%
%% Parameters - 1)A queue
%% Value - a queue with the head element removed
%% Complexity - Average case:O(1). Worst case: O(n).
%%
dequeue(Q={[],[]})->Q;
dequeue({[],R})->
	[_|T]=lists:reverse(R),
	{T,[]};
dequeue({[_|[]],R})->
	{lists:reverse(R),[]};
dequeue({[_|T],R})->{T,R}.

%%
%% @doc prepends an element to the queue.
%%
%% Parameters - 1)A queue
%%				2)An element
%% Value - a queue with both the old and new elements
%% Complexity - O(1).
%%
enqueue_front({F,R},A)->{[A]++F,R}.

%%
%% @doc removes the element at the end the queue.
%%
%% Parameters - 1)A queue
%% Value - a queue with the last element removed
%% Complexity - Average case: O(1). Worst case: O(n)
%%
dequeue_back(Q={[],[]})->Q;
dequeue_back({F,[]})->
	{F_without_last,_}=lists:split(length(F)-1,F),
	F_without_last;
dequeue_back({F,[_|T]})-> {F,T}.