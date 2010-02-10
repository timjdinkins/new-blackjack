-module(blackjack).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
	application:start(blackjack).

start(_Type, _StartArgs) ->
	% This needs to get into the supervision tree!
	{ok, _Pid} = web_server:start(),
	bj_sup:start_link().

stop(_State) ->
	ok.