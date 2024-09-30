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
-module(queue_deque).
-export([empty/1,head/1,tail/1,enqueue/2,dequeue/1,enqueue_front/2,dequeue_back/1]).


%% Complete each of these functions.

%% put add documentation here
-spec empty({list(), list()}) -> boolean().


%% put add documentation here
-spec head({list(), list()}) -> any().


%% put add documentation here
-spec tail({list(), list()}) -> any().


%% put add documentation here
-spec enqueue({list(), list()}, any()) -> {list(), list()}.


%% put add documentation here
-spec dequeue({list(), list()}) -> {list(), list()}.



-spec enqueue_front({list(), list()}, any()) -> {list(), list()}.



-spec dequeue_back({list(), list()}) -> {list(), list()}.





%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
%%
%% This is where I have chosen to put the tests for this 
%% module. They could be moved into a separate file if 
%% that made more sense.
%%
empty_test_() ->
    [
        % Happy path
        ?_assertEqual(true, empty({[], []})),  % Both lists are empty
        
        % Edge cases
        ?_assertEqual(false, empty({[1], []})),  % Front list is non-empty
        ?_assertEqual(false, empty({[], [1]})),  % Rear list is non-empty
        ?_assertEqual(false, empty({[1], [2]})), % Both lists are non-empty
        ?_assertEqual(false, empty({[1, 2, 3], []})) % Longer front list
    ].

enqueue_test_() ->
    [
        % Happy path: Adding an element to the back of the queue
        ?_assertEqual({[1], [2]}, enqueue({[1], []}, 2)),
        ?_assertEqual({[1, 2], [3]}, enqueue({[1, 2], []}, 3)),
        ?_assertEqual({[1], []}, enqueue({[], []}, 1)),

        % Edge cases
        % Adding an element to a queue where the back list is not empty
        ?_assertEqual({[1], [3, 2]}, enqueue({[1], [2]}, 3)),

        % Adding an element to a queue where the front list is empty but the back list has elements
        ?_assertEqual({[2,3],[4]}, enqueue({[], [3, 2]}, 4)),

        % Adding a complex term (a tuple)
        ?_assertEqual({[2,3],[{a,b}]}, enqueue({[], [3, 2]}, {a, b})),

        % Adding a term to a large queue
        ?_assertEqual({lists:seq(1, 100), [101]},
            enqueue({lists:seq(1, 100), []}, 101))
    ].


head_test_() ->
    [
        % Happy path
        ?_assertEqual(1, head({[1, 2, 3], [4, 5, 6]})),  % First list non-empty, returns 1
        
        % Edge cases
        ?_assertEqual(6, head({[], [4, 5, 6]})),         % First list empty, returns 6
        ?_assertEqual(nil, head({[], []})),        % Both lists empty, returns nil
        ?_assertEqual([], head({[[]], [4, 5, 6]})),      % First list contains empty list, returns []
        ?_assertEqual(42, head({[], [42]}))              % First list empty, second has one element, returns 42
    ].

tail_test_() ->
    [
        % Happy path
        ?_assertEqual(4, tail({[1, 2, 3], [4, 5, 6]})),     % Returns head of second list
        
        % Edge cases
        ?_assertEqual(nil, tail({[], []})),                 % Both lists empty, returns nil
        ?_assertEqual(3, tail({[1, 2, 3], []})),            % Second list empty, returns 3
        ?_assertEqual(10, tail({[], [10, 20, 30]})),        % First list empty, returns head of second list
        ?_assertEqual(2, tail({[1], [2]})),                 % First and second list with single element, returns head of second list
        ?_assertEqual(1, tail({[1], []}))                   % First list with single element, second list empty, returns 1
    ].


dequeue_test_() ->
    [
        % Happy path
        ?_assertEqual({[2, 3], [4, 5, 6]}, dequeue({[1, 2, 3], [4, 5, 6]})), % Dequeue first element of first list
        
        % Edge cases
        ?_assertEqual({[], []}, dequeue({[], []})),             % Both lists empty, returns unchanged
        ?_assertEqual({[2, 3], []}, dequeue({[], [3, 2, 1]})),  % First list empty
        ?_assertEqual({[6, 5, 4], []}, dequeue({[1], [4, 5, 6]})), % First list has one element
        ?_assertEqual({[], []}, dequeue({[1], []})),            % First list has one element, returns empty queue
        ?_assertEqual({[], []}, dequeue({[], [42]}))            % First list empty
    ].

enqueue_front_test_() ->
    [
        % Happy path
        ?_assertEqual({[1, 2, 3], [4, 5]}, enqueue_front({[2, 3], [4, 5]}, 1)), % Add 1 to a non-empty first list, second list unchanged
        
        % Edge cases
        ?_assertEqual({[1], []}, enqueue_front({[], []}, 1)),               % Add 1 to an empty deque
        ?_assertEqual({[1], [4, 5]}, enqueue_front({[], [4, 5]}, 1)),       % Add 1 to an empty first list, second list non-empty
        ?_assertEqual({[1, 2], []}, enqueue_front({[2], []}, 1)),           % Add 1 to a first list with one element
        ?_assertEqual({[{foo}, 2, 3], []}, enqueue_front({[2, 3], []}, {foo})) % Add a non-integer element to the first list
    ].

dequeue_back_test_() ->
    [
        % Happy path
        ?_assertEqual({[1, 2, 3], [5]}, dequeue_back({[1, 2, 3], [4, 5]})), % Removes the head of the second list

        % Edge cases
        ?_assertEqual({[], []}, dequeue_back({[], []})),                % Both lists empty, returns unchanged
        ?_assertEqual({[1, 2], []}, dequeue_back({[1, 2, 3], []})),     % Removes the last element of the first list
        ?_assertEqual({[], []}, dequeue_back({[1], []})),               % Removes the only element from the first list
        ?_assertEqual({[], []}, dequeue_back({[], [42]})),              % Removes the only element from the second list
        ?_assertEqual({[], [43, 44]}, dequeue_back({[], [42, 43, 44]})) % Removes the head of the second list, keeps the rest
    ].

-endif.