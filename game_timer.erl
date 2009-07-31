-module(game_timer).
-behavior(gen_fsm).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, timing/2]).

-export([set_timeout/2, cancel_timeout/1]).

start() ->
	gen_fsm:start(?MODULE, [], []).
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	{ok, waiting, []}.

terminate(_Reason, _StateName, _Data) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_timeout(Pid, Payload) ->
	gen_fsm:send_event(Pid, {set_timer, Payload}).

cancel_timeout(Pid) ->
	gen_fsm:send_event(Pid, cancel_timeout).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

waiting({set_timer, {Secs, Fun}}, []) ->
	Ref = start_timer(Secs),
	{next_state, timing, {Ref, Fun}}.

timing({timeout, _Ref, _Msg}, {_Ref, Fun}) ->
	Fun(),
	{next_state, waiting, []};

timing({set_timer, {Secs, Fun}}, {OldRef, _Fun}) ->
	gen_fsm:cancel_timer(OldRef),
	Ref = start_timer(Secs),
	{next_state, timing, {Ref, Fun}};

timing(cancel_timeout, {OldRef, _Fun}) ->
	gen_fsm:cancel_timer(OldRef),
	{next_state, waiting, []}.

handle_event(stop, _StateName, {Ref, _Fun}) ->
	gen_fsm:cancel_timer(Ref),
	{stop, normal, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_timer(Secs) ->
	gen_fsm:start_timer(Secs * 1000, []).