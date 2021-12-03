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
-module(ral).
-export([get/2,update/4,cons/2]).


%%
%%A function used to retrieve a specified element from the RAL.
%%
%% Parameters: Ral - the random access list containing the element
%%			   Index - the index in the random access list of the desired element
%% Value: the element at the specified index or none if there is no such index
%% Complexity: Best Case - O(1)
%% 			   Average Case - O(log n)
%%
get([],_Index)->
	none;
get(_Ral,Index) when Index < 0 ->
	none;
get([{Element,none,none}|_T],0)->
	Element;
get(Ral,Index)->
	{Skipped_indices,Tree} = search(Ral,Index,0),
	case Tree == none of
		true -> 
			none;
		_ ->
			{Leaf_count,_,_} = Tree,
			find(Tree,build_bin_list(Index-Skipped_indices,Leaf_count))
	end.

%%
%%This is a helper function used to find tree
%%traversals to some specified index of the list.
%%
%%This function converts a positive integer
%%to binary for some bit-space, M. The binary number is
%%represented as a list. The 
%%list will contain only as many bits as needed for
%%the given bit-space.
%%
%% Parameters: N - the number to represent
%%             M - the bit space for the number.
%% Value: a list of 1s and 0s representing N in binary.
%% Complexity: O(log n).
%%
build_bin_list(_,1)->
	[];
build_bin_list(N,Power2) ->
	Lower_power = Power2 div 2,
	case N >= Lower_power of
		true->
			[1]++build_bin_list(N-Lower_power,Lower_power);
		_->
			[0]++build_bin_list(N,Lower_power)
	end.

%%
%%This is a helper function that searches through the list
%%until the tree containing the index is found.
%%
%% Parameters: RAL - a Random Access List
%%             Index - the index in the list
%%             Accum - an accumulator for how many indices were skipped over
%%                     as each tree was found not to contain the index needed.
%%                     This is initialized to 0.
%% Value: a tuple whose first element is the total number of indices skipped
%%        and who's second index is the tree containing the value at the Index parameter.
%% Complexity: O(log n)
%%
search([],_Index,_Accum)->{fail,none};
search([H|_T],0,_)-> 
	{0,H};
search([{_,none,none} | T],Index,Accum)->
	search(T,Index,Accum+1);
search([{Leaf_count,_,_}|T],Index,Accum) when Leaf_count < Index-Accum->
	search(T,index, Accum+Leaf_count);
search([H|_],_,Accum)->
	{Accum,H}.

%%
%%A helper function used to retrieve the stored element in a Tree's specified leaf.
%%
%% Parameters: Tree - the tree to traverse
%%             Traversal_list - a list to the desired leaf 
%%								where each element is a 0 or a 1
%%                              with 0 indicating go left and 1 go right
%% Value: an element stored in the specified leaf of the tree.
%% Complexity: O(log n)
%%
find(none,_Traversal_list)->
	none;
find({Element,_,_},[])->
	Element;
find({_,Next_l,_},[0|T])->
	find(Next_l,T);
find({_,_,Next_r},[1|T])->
	find(Next_r,T).


%%
%%A function used to change a value stored at a specified index.
%%
%% Parameters: Ral - the source list for the change
%% 			   Index - the index of the element to be changed
%%			   Value - the new value for the given index
%%			   Accum - the number of indices skipped during the search process.
%%			           This must be 0 when update is called.
%% Value: a list with the specified element changed and the others unchanged
%% Complexity: Best Case - O(1)
%%  		   Average Case - O(log n)
%%
update(none,_Index,_Element,_Accum)->none;
update([],_,_,_)->[];
update(Ral,Index,_,_) when Index < 0->Ral;
update([{_,none,none}|T],0,Element,_)->
	[{Element,none,none}]++T;
update([H={_,none,none}|T],Index,Element,Accum) when Index > 0 ->
	[H]++update(T,Index,Element,Accum+1);
update([H = {Leaf_count,_,_}|T],Index,Element,Accum) when Leaf_count<Index-Accum->
	[H]++update(T,Index,Element,Accum+Leaf_count);
update([H = {Leaf_count,_,_}|T],Index,Element, Accum)->
	case Bin_list = build_bin_list(Index-Accum,Leaf_count) of
		fail->
			[H]++T;
		_->
			[replace(H,Bin_list,Element)]++T
	end.

%%
%%A helper function that builds an appropriate tree with the specified value changed.
%%
%% Parameters: Tree - the tree who's leaf contains the item to be changed.
%%			   Traversal_list - a list to the desired leaf 
%%								where each element is a 0 or a 1
%%                              with 0 indicating go left and 1 go right
%%			   Element - the new element to be stored in the leaf
%% Value: a tree containing the new element in the specified leaf and all other elements
%%        in their unchanged leaves
%% Complexity: O(log n)
%%
replace({_Old_Element,none,none},[],Element)->
	{Element,none,none};
replace({Leaf_count,Next_l,Next_r},[0|T],Element)->
	{Leaf_count,replace(Next_l,T,Element),Next_r};
replace({Leaf_count,Next_l,Next_r},[_|T],Element)->
 	{Leaf_count,Next_l,replace(Next_r,T,Element)}.


%%A function to prepend an element to a random access list
%%
%% Parameters: Element - the element to prepend to the list
%% 			   Ral - the random access list to which the element is to be prepended
%% Value: a random access list with the specified element at index 0
%% Complexity: O(1)
 cons(none,Ral)->Ral;
 cons(Element,none)->
 	[{Element,none,none}];
 cons(Element,[])->
 	cons(Element,none);
 cons(Element,[H={_,none,none}|T])->
 	link({2,{Element,none,none},H},T);
 cons(Element,Ral)->
 	[{Element,none,none}]++Ral.

%%
%%A helper function used to ensure no two trees have the same
%%number of leaves.
%%
%% Parameters: Tree - a tree to link
%%             Trees - the remaining trees in the random access list
%% Value: a random access list updated such that no two trees have
%% 		  the same number of leaves and all trees have 2^n leaves, n=(0,1,2,...).
 link(Tree,[])->
 	[Tree];
 link(Tree = {Leaf_count,_,_},[H={Head_leaf_count,_,_}|T])->
 	link({Leaf_count+Head_leaf_count,Tree,H},T).


