-module(bj_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	case bj_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

stop(_State) ->
	ok.