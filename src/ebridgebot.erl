-module(ebridgebot).
-compile(export_all).

-include("ebridgebot.hrl").

link_room(BotId, TgGroupId, MucJid) ->
	Pid = ebridgebot_tg_component:pid(BotId),
	Pid ! {link_rooms, TgGroupId, MucJid}.
get_bot_name_by_id(BotId) ->
	Bots = application:get_env(?MODULE, bots, []),
	deep_search([BotId, name], Bots).

gen_uuid() ->
	list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

run_test() ->
	run_test(ebridgebot_component_SUITE, []).
run_test(Suite) ->
	run_test(Suite, []).
run_test(Suite, Testcase) when is_atom(Testcase) ->
	run_test(Suite, [Testcase]);
run_test(Suite, Testcases) when is_atom(Suite) ->
	case application:get_env(ebridgebot, test_path, []) of
		[] -> ct:print("ERROR: test_path not found. Set test_path in ejabberd section of sys.config", []),
			{error, invalid_test_path};
		TestPath ->
			ct:run_test(
				[{testcase, T} || T <- Testcases] ++
				[{suite, Suite},
					{dir, filename:join(TestPath, "test")},
					{include, ["_build/dev/lib/escalus", "include"]},
					{config, filename:join(TestPath, "test/test.config")}])
	end.

deep_search(Path, List) ->
	deep_search(Path, 1, List).
deep_search([], _N, List) -> List;
deep_search([Key | Tail], N, [Tuple | _] = List) when is_tuple(Tuple) ->
	case lists:keyfind(Key, N, List) of
		{_, [_ | _] = Value} when Tail /= [] ->
			deep_search(Tail, Value);
		{_, Value} -> Value;
		_ -> []
	end.

tag_decorator([], Data, Mod, Fun) ->
	fun() -> Mod:Fun(Data) end;
tag_decorator([El | TEls], [Pkt | _] = Data, Mod, Fun) ->
	case xmpp:get_subtag(Pkt, El) of
		false -> tag_decorator(TEls, Data, Mod, Fun);
		Tag -> fun() -> Mod:Fun(Tag, Data) end
	end.