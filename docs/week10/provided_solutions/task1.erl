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
-module(task1).
-export([exercise/0]).


exercise()->
	Small = [{5,none,none},{2,{8,none,none},{-3,none,none}}],
	Small_pair = [{2,{8,none,none},{-3,none,none}}],
	Small_single = [{5,none,none}],
	Empty = [],

	%%Exercising cons
	[{4,
  {2,{7,none,none},{5,none,none}},
  {2,{8,none,none},{-3,none,none}}}] = ral:cons(7,Small),

  [{7,none,none},{2,{8,none,none},{-3,none,none}}] = ral:cons(7,Small_pair),

  [{2,{7,none,none},{5,none,none}}] = ral:cons(7,Small_single),

  [{7,none,none}] = ral:cons(7,[]),

  %%Exercising get
  none = ral:get(Empty,0),
  none = ral:get(Small,-3),
  none = ral:get(Small, 17),
  5 = ral:get(Small,0),
  8 = ral:get(Small,1),
  -3 = ral:get(Small,2),

  8 = ral:get(Small_pair,0),
  -3 = ral:get(Small_pair,1),

  5 = ral:get(Small_single,0),

  %Exercising update
   none = ral:update(none,0,7,0),
   [] = ral:update([],0,11,0),
  [{15,none,none},{2,{8,none,none},{-3,none,none}}] = ral:update(Small,0,15,0),
  [{5,none,none},{2,{15,none,none},{-3,none,none}}] = ral:update(Small,1,15,0),
  [{5,none,none},{2,{8,none,none},{15,none,none}}] = ral:update(Small,2,15,0),
  [{5,none,none},{2,{8,none,none},{-3,none,none}}] = ral:update(Small,12,15,0),
  it_worked.

