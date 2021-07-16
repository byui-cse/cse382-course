-module(task2).
-export([execute/0]).

execute()->

	%Generate a bunch of simulated data.

	Regions = [europe,north_america,central_america,south_america,africa,south_asia,east_asia],
	Years = lists:seq(2017,2021),
	%The elements of the tuple are Month, Year, Region, and Sales in that order.
	Sales = [{M,Y,R,rand:uniform()*10_000_000} || M<-lists:seq(1,12), Y<-Years, R<-Regions],


	%Here are example links to execute against the data.
	Generate_annual_totals = [
		%Group by region and year
		fun
			(Ungrouped) -> [{Region,Year, lists:filter(fun({_,Y,R,_}) -> (R == Region) and (Y==Year)  end, Ungrouped) } 
			|| Region <- Regions, Year <- Years] 
		end,
		%Reduce all of the sales for each region-year pair to a sum for that pair.
		fun
			(Grouped)->[{R, Y, lists:foldl(fun({_,_,_,S},Accum)->Accum + S end,0,G)} || {R,Y,G}<- Grouped]
		end
	],

	%I've chosen fold since it uses less CPU resources than the chain function.
	lists:foldl(fun (Link,Value)-> Link(Value) end,Sales,Generate_annual_totals).