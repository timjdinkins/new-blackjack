-module(web_server).

-export([start/0, start/1, stop/0, dispatch_request/2]).

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
listen(Req, Pid) ->
	player:register_proxy(Pid, self()),
	receive
		timeout -> json_ok(Req, {action, "reconnect"});
		{'$gen_cast', Msgs} ->
			io:format("Message from Player in listen loop: ~p~n", [Msgs]),
			json_ok(Req, Msgs);
		Any ->
			io:format("Unknown message in listen loop: ~p~n", [Any]),
			json_ok(Req, {unknown, Any})
	end.

handle_action("register", Req) ->
	Name = wh:get_param(Req, "name"),
	case registry:register(Name) of
		{ok, _Pid} -> json_ok(Req, {ok, "registered"});
		{error, Reason} -> json_ok(Req, {error, Reason})
	end;

handle_action("listen", Req) ->
	Name = wh:get_param(Req, "name"),
	case registry:get_pid(Name) of
		{ok, Pid} ->
			timer:send_after(30000, self(), timeout),
			listen(Req, Pid);
		{error, Reason} ->
			json_error(Req, Reason)
	end;

handle_action("action", Req) ->
	Name = wh:get_param(Req, "name"),
	Action = wh:get_param(Req, "action"),
	io:format("Action request for: ~p -> ~p~n", [Name, Action]),
	case registry:get_pid(Name) of
		{ok, Pid} ->
			case Action of
				"join_table" -> join_table(Pid, Req);
				"bet" -> bet(Pid, Req);
				"hit" -> hit(Pid, Req);
				"stay" -> stay(Pid, Req)
			end,
			json_ok(Req, {ok, "ok"});
		{error, Reason} ->
			json_error(Req, {error, Reason})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join_table(Pid, _Req) ->
	player:join_table(Pid).

bet(Pid, Req) ->
	Amt = wh:get_param(Req, "amt"),
	io:format("Betting: ~p -> ~p~n", [Pid, Amt]),
	player:bet(Pid, list_to_integer(Amt)).

hit(Pid, _Req) ->
	player:hit(Pid).

stay(Pid, _Req) ->
	player:stay(Pid).

json_ok(Req, Msgs) when is_list(Msgs) -> json_respond(Req, <<"ok">>, Msgs);
json_ok(Req, Msgs) -> json_respond(Req, <<"ok">>, [Msgs]).

json_error(Req, Msgs) when is_list(Msgs) -> json_respond(Req, <<"error">>, Msgs);
json_error(Req, Msgs) -> json_respond(Req, <<"error">>, [Msgs]).

json_respond(Req, Status, Msgs) ->
	io:format("json_respond =>> ~p~n", [Msgs]),
	Req:ok({"text/json", [], json_msg(Status, Msgs)}).

json_msg(Status, Msgs) ->
	lists:flatten(rfc4627:encode({obj, [{"status", Status}|Msgs]})).