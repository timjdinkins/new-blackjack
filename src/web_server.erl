-module(web_server).

-export([start/1, stop/0, dispatch_requests/1]).

-define(OK, <<"ok">>).

start(Port) ->
	mochiweb_http:start([{port, Port}, {loop, fun dispatch_requests/1}]).

stop() ->
	mochiweb_http:stop().

dispatch_requests(Req) ->
	Path = Req:get(path),
	Action = clean_path(Path),
	handle(Action, Req).

handle("/register", Req) ->
	Params = Req:parse_qs(),
	Name = proplists:get_value("name", Params),
	case web_players:register(Name) of
		{ok, Pid} ->
			respond(Req, 200, subst("Pid: ~s", [pid_to_list(Pid)]));
		{error, Msg} ->
			respond(Req, 200, subst("Error: ~s", [Msg]))
	end;

handle("/join-table", Req) ->
	Params = Req:parse_qs(),
	Name = proplists:get_value("name", Params),
	Seat = proplists:get_value("seat", Params),
	case web_players:get_pid(Name) of
		{ok, Pid} ->
			player:find_table(Pid),
			player:join_table(Pid, list_to_atom(Seat)),
			respond(Req, 200, subst("OK", []));
		{error, Msg} ->
			respond(Req, 200, subst("Error: ~s", [Msg]))
	end;

handle(Unknown, Req) ->
	respond(Req, 404, subst("Unknown action: ~s", [Unknown])).

respond(Req, Code, Msg) ->
	Req:response({Code, [{"Content-Type", "text/plain"}], Msg}).

subst(Template, Values) when is_list(Values) ->
	list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.