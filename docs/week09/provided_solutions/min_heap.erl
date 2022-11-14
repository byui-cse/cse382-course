-module(min_heap).
-export([get_min/1,insert/2,merge/2,remove_min/1,is_empty/1]).
-type heap() :: {integer(),any(),heap(),heap()}.

-spec is_empty(heap()) -> boolean().
is_empty(nil)->true;
is_empty(_Heap)->false.

-spec get_min(heap()) -> any() | nil.
get_min(nil)->nil;
get_min({_Rank,Value,_left,_right})->Value.

-spec remove_min(heap()) -> heap().
remove_min({_Rank,_Value,Next_l,Next_r})->
	merge(Next_l,Next_r).

-spec insert(any(),heap()) -> heap() | nil.
insert(nil,nil)->nil;
insert(Value,Heap)->
	merge({1,Value,nil,nil},Heap).

-spec rank(heap()) -> integer().
rank(nil)->0;
rank({Rank,_Value,_Next_l,_Next_r})->Rank.


-spec build_node(any(),heap(),heap()) -> heap().
build_node(X,A,B)->
	case rank(A) >= rank(B) of
		true->{rank(B)+1,X,A,B};
		false->{rank(A)+1,X,B,A}
	end.

-spec merge(heap(),heap()) -> heap().
merge(nil, Heap)->Heap;
merge(Heap,nil)->Heap;
merge({_Rank_a,Value_a,Next_al,Next_ar},{_Rank_b,Value_b,_Next_bl,_Next_br}=B) when Value_a =< Value_b ->
	build_node(Value_a,Next_al,merge(Next_ar,B));
merge({_Rank_a,_Value_a,_Next_al,_Next_ar}=A,{_Rank_b,Value_b,Next_bl,Next_br})->
	build_node(Value_b,Next_bl,merge(A,Next_br)).









