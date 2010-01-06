-module(web_server).

-export([start/1, stop/0, dispatch_request/1]).

start(Port) ->
	mochiweb_http:start([{port, Port}, {loop, fun dispatch_request/1}]).

stop() ->
	mochiweb_http:stop().

json_ok(Req, Msgs) -> json_respond("ok", Msgs, Req).
json_error(Req, Msgs) -> json_respond("error", Msgs, Req).

json_respond(Status, Msgs, Req) -> Req:ok({"text/json", [], json_msg(Status, Msgs)}).
json_msg(Status, Msgs) ->
	lists:flatten(rfc4627:encode({obj, [{"status", <<Status>>}, Msgs]})).

listen(Req, SID) ->
	player:listen(SID, self()),
	receive
		timeout -> json_ok(Req, {action, reconnect});
		{messages, Msgs} -> json_ok(Req, Msgs)
	end.

timeout_listen(Pid) -> Pid ! timeout.

dispatch_request(Req) ->
	Path = Req:get(path),
	Action = clean_path(Path),
	handle_action(Action, Req).

handle_action("/bet", Req) ->
	SID = get_sid(Req),
	Amt = get_param(Req, "amt"),
	send_player(SID, {bet, Amt}),
	json_ok(Req, {response, ok});

handle_action("/listen", Req) ->
	SID = get_sid(Req),
	timer:apply_after(30000, ?MODULE, timeout_listen, [self()]),
	wait(Req, SID);

handle_action("/register", Req) ->
	Name = get_param(Req, "name"),
	% Impliment This!!!
	SID = player_registry:generate_sid(),
	{ok, Pid} = player:start(SID, Name, 200),
	player_registry:register_sid(SID, Pid),
	json_ok(Req, {sid, <<SID>>}).

clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.

send_player(SID, Msg) ->
	% Impliment This!!!
	Pid = player_registry:player_pid_from_sid(SID),
	player:msg_from_web_client(Pid, Msg).

get_param(Req, ValName) ->
	proplists:get_value(ValName, Req:parse_qs()).

get_sid(Req) ->
	get_param(Req, "sid").