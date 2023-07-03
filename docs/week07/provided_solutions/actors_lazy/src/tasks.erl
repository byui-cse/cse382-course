
%%%-------------------------------------------------------------------
%%% @author Lee Barney
%%% @copyright 2023 Lee Barney licensed under the <a>
%%%        rel="license"
%%%        href="http://creativecommons.org/licenses/by/4.0/"
%%%        target="_blank">
%%%        Creative Commons Attribution 4.0 International License</a>
%%%
%%% Project - CSE 382
%%%
%%% @doc
%%% These solutions are not intended to be ideal solutions. Instead,
%%% they are solutions that you can compare against yours to see
%%% other options and to come up with even better solutions.
%%%
%%% You can find the unit tests to be passed near the bottom of this 
%%% file, just before the speed tests.
%%% @end
%%% Created : 10 May 2023 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------

-module(tasks).

%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([start_rand_stream/1,next_rand/1,
		 start_cipher_stream/1,decipher/2,enqueue/2,dequeue/1]).


%%
%% The rand_stream stateful actor functions and the rand_stream client function.
%% 

%%
%% This is a facade function that starts a stateful actor implementation of a pseudo-random
%% stream of integers.
%%
%% Parameter - Seed: an initial value that predetermines which stream of integers are produced.
%% Value - the process ID of the stream.
%% Complexity - O(1)
%%
start_rand_stream(Seed)->
	spawn(tasks,rand_stream,[Seed]).

%%
%% This is a is a client function for the rand_stream stateful actor. 
%% stream of integers.
%%
%% Parameter - Stream_pid: the process ID of a previously started rand_stream actor.
%% Value - the next pseudo-random integer retrieved from the rand_stream. 
%% This function has a call-type behavior. It makes a request to the process and 
%% waits for the response.
%% Complexity - O(1)
%%
next_rand(Stream_pid)->
	Stream_pid ! self(),
	receive
		Rand -> Rand
	end.

%%
%% This pseudo-random number stream is implemented using the stateful actor pattern. The state consists of
%% the previous pseudo-random integer or an initial seed value for the stream. A few hard coded values are
%% used by this stream to create a simple LCG pseudo-random number generator. They are defined as
%%
%% a (multiplier) = 1103515245,
%% c (incrementer) = 12345, and
%% m (modulus) = 2147483648.
%% 
%% The next pseudo-random number and seed is calculated as (a*seed+c) (mod m).
%% This is the same set of numbers used in the C standard library's pseudo-random number stream as 
%% described in https://en.wikipedia.org/wiki/Linear_congruential_generator.
%%
%% Parameter - an integer that is the initial seed used to generate a stream of random numbers.
%% Value - 
%% element of the parameter tuple followed by the second element of the 
%% parameter tuple. The second element of the value tuple is an empty list.
%% Complexity - O(1)
%%

rand_stream(Seed)->
	Next_seed = receive
		Pid ->
			Rand = (1103515245*Seed+12345)rem 2147483648,%% C standard library values used by GCC.
			Pid ! Rand,
			Rand %the value of the receive fun is stored in Next_seed
		end,
	rand_stream(Next_seed).


%%
%% The cipher_stream stateful actor and its client functions.
%% 

%%
%% start_cipher_stream is a facade function that spawns a Ceasar stream.
%%
%% Parameters -  1) Shift_amount: an integer amount to be added to each character
%%				    of the message
%% Value - the process ID of the cipher stream process
%% Complexity - O(n) where n is the number of characters in the message.
%%
start_cipher_stream(Shift_amount)->
	spawn(tasks,cipher_stream,[{Shift_amount,[]}]).


%%
%% decipher is a function that unshifts a Ceasar encyphered message by a specified amount.
%%
%% Parameters -  1) Message: the ceasar cyphered message to be deciphered
%%				 2) Unshift_amount: an integer amount to be subtracted from each character
%%				    of the message
%% Value - the deciphered text
%% Complexity - O(n) where n is the number of characters in the message.
%%
decipher(Message,Unshift_amount)->
	lists:map(fun(Character)-> Character - Unshift_amount end,Message).

%%
%% enqueue is a client function for the cipher_stream stateful actor. Without waiting for a value, 
%% enqueue sends a message to a cipher_stream to be enciphered and enqueued. The message then waits
%% in the cipher_stream until it is requested. This function has a cast-like behavior.
%%
%% Parameters -  1) Message: the text to be enciphered
%%				 2) Cipher_pid: the process ID of the cipher_stream stateful actor that holds the 
%%				 	enciphered message
%% Value - none
%% Complexity - O(1).
%%
enqueue(Message,Cipher_pid)->
	Cipher_pid ! {put,Message}.

%%
%% dequeue is a client function for the cipher_stream stateful actor. It retrieves the next
%% enciphered message placed in the cipher_stream by some other process. This function has a
%% send-like behavior.
%%
%% Parameter -  Cipher_pid: the process ID of the cipher_stream stateful actor that holds 
%%				 	enciphered messages
%% Value - the next enciphered message
%% Complexity - O(1).
%%
dequeue(Cipher_pid)->
	Cipher_pid ! {self(),get},
	receive
		Result -> Result
	end.

%%
%% This cipher stream is implemented using the stateful actor pattern. The state consists of
%% the positive or negative integer amount to shift each element in a message during encipherment. 
%% Each cipher stream has a queue to which messages are appended after they are enciphered.
%% Each time an enciphered message is desired, the head of the queue is 
%% removed and sent to the requester. It is the responsibility of the requester
%% to know how to decipher the message.
%%
%% A bi-directional enciphered messaging system would consist of two cipher_stream actors. Any message 
%% sent to user 2 from user 1 would be put into cipher_stream A by user 1. The enciphered messages
%% would be pulled from cipher_stream A by user 2 and then deciphered. Any message sent to user 1 from
%% user 2 would be put in cipher_stream B by user 2. The enciphered messages would be pulled from
%% cipher_stream B by user 1 and then deciphered.
%%
%% Parameter -  State: a 2-tuple consisting of
%%				 1) Shift_amount: an integer amount used to modify each integer in every message, and
%%				 2) Messages: the queue to which enciphered messages are added.
%% Value - none
%% Complexity - Adding a message is O(n) where n is the length of the message in characters. Retrieving
%% 				an enciphered message is O(1).
%%

cipher_stream({Shift_amount,Messages})->
	Next_state = receive
		{put,Message} ->
				{Shift_amount,Messages++[lists:map(fun(Character)-> Character + Shift_amount end,Message)]};
		{Pid,get} ->
			[H|T] = Messages,
			Pid ! H,
			{Shift_amount,T}
	end,
	cipher_stream(Next_state).



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%
%%% These are the component-level tests for the random stream
%%%
random_stream_test_() ->
    {setup,
     fun() -> %this setup fun is run once before the tests are run. If you want setup and teardown to run for each test, change {setup to {foreach
        Rand_stream_pid = start_rand_stream(10),%spawn the process
        register(rand,Rand_stream_pid) %register a local name for the process so the process can be used from anywhere
        
     end,
     fun(_) ->%This is the teardown fun. No teardown needs to be done.
     	nil
     end,
    [%This is the list of tests to be generated and run.
        ?_assertEqual(297746555, next_rand(rand)),
        ?_assertEqual(1849040536, next_rand(rand)),
        ?_assertEqual(736986865, next_rand(rand)),
        ?_assertEqual(581309142, next_rand(rand)),
        ?_assertEqual(1106733399, next_rand(rand))

    ]}.

%%%
%%% These are the component-level tests for the cipher stream
%%%
cipher_stream_enqueue_dequeu_test_() ->
    {setup,
     fun() -> %this setup fun is run once before the tests are run. If you want setup and teardown to run for each test, change {setup to {foreach
        Cipher_stream_pid = start_cipher_stream(3),
        register(ceasar,Cipher_stream_pid)
     end,
     fun(_) ->%This is the teardown fun. No teardown needs to be done.
     	nil
     end,
    [%This is the list of tests to be generated and run.
    	?_assertEqual({put,"hello"}, enqueue("hello",ceasar)),
    	?_assertEqual({put,"goodbye"}, enqueue("goodbye",ceasar)),
    	?_assertEqual({put,"like this, ~name"}, enqueue("like this, ~name",ceasar)),
    	?_assertEqual({put,""}, enqueue("",ceasar)),
    	?_assertEqual({put,[]}, enqueue([],ceasar)),
        ?_assertEqual("khoor", dequeue(ceasar)),
        ?_assertEqual("jrrge|h", dequeue(ceasar)),
        ?_assertEqual([111,108,110,104,35,119,107,108,118,47,35,129,113,100,112,104], dequeue(ceasar)),
        ?_assertEqual([], dequeue(ceasar)),
        ?_assertEqual([], dequeue(ceasar))

    ]}.

-endif.



	