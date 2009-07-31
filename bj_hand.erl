-module(bj_hand).

-export ([compute/1]).

compute([H|T]) ->
	{_, _, Val} = H,
	case Val of
		11 -> AceCount = 1;
		Val -> AceCount = 0
	end,
	compute(T, Val, AceCount);
compute([]) ->
	0.

compute([H|T], Tot, AceCount) ->
	{_, _, Val} = H,
	case Val of
		11 -> NewAceCount = AceCount + 1;
		Val -> NewAceCount = AceCount
	end,
	compute(T, Val + Tot, NewAceCount);
compute([], Tot, AceCount) when Tot > 21, AceCount > 0 ->
	NewTot = Tot - 10,
	NewAceCount = AceCount - 1,
	compute([], NewTot, NewAceCount);
compute([], Tot, _AceCount) ->
	Tot.