-module(web_server).

-export([start/0, start/1, stop/0, dispatch_request/2]).
-export([test/0]).

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
	io:format("Registered listener ~p.~n", [self()]),
	receive
		{'$gen_cast', Msgs} ->
			io:format("Sending message: ~p~n", [Msgs]),
			json_ok(Req, Msgs);
		{error, socket_closed} ->
			% The web browser closes or goes to a different page.
			ok;
		timeout ->
			io:format("Timed out ~p.~n", [self()]),
			json_ok(Req, wh:enc_msg("reconnect"));
		Any ->
			io:format("Unknown message: ~p~n", [Any]),
			json_ok(Req, wh:enc_msg(Any))
	end,
	timer:cancel(TRef).

handle_action("register", Req) ->
	IP = Req:get_header_value("host"),
	Name = wh:get_param(Req, "name"),
	% This is busted.  The cookie code can't handle the hash as it
	% wants it escaped.
	SID = Name ++ wh:generate_sid(Name, IP),
	Cookie = mochiweb_cookies:cookie("sid", SID, [{path, "/"}]),
	io:format("Cookie: ~p~n", [Cookie]),
	
	case registry:register_player(SID, Name) of
		{ok, _Pid} ->
			Req:ok({"text/json", [Cookie], json_msg(<<"ok">>, wh:enc_msg("registered"))});
		{error, Reason} -> json_ok(Req, wh:enc_error(Reason, []))
	end;

handle_action("listen", Req) ->
	SID = Req:get_cookie_value("sid"),
	case registry:get_pid(SID) of
		{ok, Pid} ->
			{ok, TRef} = timer:send_after(30000, self(), timeout),
			listen(Req, Pid, TRef);
		{error, Reason} ->
			json_error(Req, wh:enc_error(Reason))
	end;

handle_action("action", Req) ->
	SID = Req:get_cookie_value("sid"),
	Action = wh:get_param(Req, "a"),
	io:format("Action request for: ~p -> ~p~n", [SID, Action]),
	case registry:get_pid(SID) of
		{ok, Pid} ->
			case Action of
				"join_table" ->
					{ok, Seat} = join_table(Pid, Req),
					json_ok(Req, wh:enc_registered(Seat));
				"bet" ->
					bet(Pid, Req),
					json_ok(Req, wh:enc_msg("ok"));
				"hit" ->
					hit(Pid, Req),
					json_ok(Req, wh:enc_msg("ok"));
				"stay" ->
					stay(Pid, Req),
					json_ok(Req, wh:enc_msg("ok"))
			end;
		{error, Reason} ->
			json_error(Req, wh:enc_error(Reason))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join_table(Pid, _Req) ->
	player:join_table(Pid).

bet(Pid, Req) ->
	Amt = wh:get_param(Req, "amt"),
	io:format("Bet of: ~p~n", [Amt]),
	player:bet(Pid, list_to_integer(Amt)).

hit(Pid, _Req) ->
	player:hit(Pid).

stay(Pid, _Req) ->
	player:stay(Pid).

json_ok(Req, Msgs) -> json_respond(Req, <<"ok">>, Msgs).

json_error(Req, Msgs) -> json_respond(Req, <<"error">>, Msgs).

json_respond(Req, Status, Msgs) -> Req:ok({"text/json", [], json_msg(Status, Msgs)}).

json_msg(Status, Msgs) ->
	Ret = lists:flatten(rfc4627:encode({obj, [{status, Status}, {msgs, Msgs}]})),
	io:format("JSON-Msg: ~p~n", [Ret]),
	Ret.

test() ->
	Ms = wh:enc_msg("Start the DAMN GAME!", []),
	NMs = wh:enc_msg("Run for it!", Ms),
	NNMs = wh:enc_bust(29, NMs),
	V = json_msg(<<"ok">>, NNMs),
	io:format("Out -> ~p~n", [V]).