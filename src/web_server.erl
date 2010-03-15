-module(web_server).

-export([start/0, start/1, stop/0, dispatch_request/2]).
-export([json_msg/2]).

start() ->
	start(9000).

start(Port) ->
	{_File, Path} = code:is_loaded(?MODULE),
	DocRoot = filename:join([filename:dirname(filename:dirname(Path))|["public_html"]]),
	mochiweb_http:start([{port, Port}, {loop, fun(Req) -> dispatch_request(Req, DocRoot) end}]).

stop() ->
	mochiweb_http:stop().

dispatch_request(Req, DocRoot) ->
	"/" ++ Path = Req:get(path),
	case Req:get(method) of
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			Req:serve_file(Path, DocRoot);
    'POST' ->
			Action = wh:clean_path(Path),
			handle_action(Action, Req);
		_ ->
			Req:respond({501, [], []})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Main Loop %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen(Req, Pid, TRef) ->
	player:register_proxy(Pid, self()),
	receive
		{'$gen_cast', Msgs} ->
			io:format("Msg to client: ~p~n", [Msgs]),
			json_ok(Req, Msgs);
		{error, socket_closed} -> % The web browser closes or goes to a different page.
			ok;
		timeout ->
			ok(Req)
	end,
	timer:cancel(TRef).

handle_action("register", Req) ->
	IP = Req:get_header_value("host"),
	Name = wh:get_param(Req, "name"),
	SID = wh:generate_sid(Name, IP),
	Cookie = mochiweb_cookies:cookie("sid", SID, [{path, "/"}]),
	io:format("Cookie: ~p~n", [Cookie]),
	
	case registry:register_player(SID, Name) of
		{ok, _Pid} ->
			io:format("Sending response...", []),
			Req:ok({"text/json", [Cookie], []});
		{error, Reason} ->
			json_ok(Req, wh:enc(error, {reason, Reason}))
	end;

handle_action("listen", Req) ->
	SID = Req:get_cookie_value("sid"),
	case registry:get_pid(SID) of
		{ok, Pid} ->
			{ok, TRef} = timer:send_after(30000, self(), timeout),
			listen(Req, Pid, TRef);
		{error, Reason} ->
			json_error(Req, wh:enc(error, {reason, Reason}))
	end;

handle_action("action", Req) ->
	SID = Req:get_cookie_value("sid"),
	Action = wh:get_param(Req, "a"),
	case registry:get_pid(SID) of
		{ok, Pid} ->
			case Action of
				"join_table" ->
					{ok, Seat} = join_table(Pid, Req),
					io:format("Responding to Join Table with Seat: ~p~n", [Seat]),
					json_ok(Req, wh:enc(reg, {seat, Seat}));
				"bet" ->
					bet(Pid, Req),
					ok(Req);
				"hit" ->
					hit(Pid, Req),
					ok(Req);
				"stay" ->
					stay(Pid, Req),
					ok(Req)
			end;
		{error, Reason} ->
			json_error(Req, wh:enc(error, {reason, Reason}))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join_table(Pid, _Req) ->
	player:join_table(Pid).

bet(Pid, Req) ->
	Amt = wh:get_param(Req, "amt"),
	player:bet(Pid, list_to_integer(Amt)).

hit(Pid, _Req) ->
	player:hit(Pid).

stay(Pid, _Req) ->
	player:stay(Pid).
	
ok(Req) -> Req:ok({"text/json", [], []}).

json_ok(Req, Msgs) -> json_respond(Req, <<"ok">>, Msgs).

json_error(Req, Msgs) -> json_respond(Req, <<"error">>, Msgs).

json_respond(Req, Status, Msgs) -> Req:ok({"text/json", [], json_msg(Status, Msgs)}).

json_msg(Status, Msgs) ->
	Ret = mochijson2:encode({struct, [{status, Status}, {msgs, Msgs}]}),
	% io:format("JSON-Msg: ~p~n", [Ret]),
	Ret.