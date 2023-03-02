-module(ebridgebot).
-compile(export_all).

-include("ebridgebot.hrl").

link_room(GroupId, MucJid, BotName) ->
	mnesia:dirty_write(#ebridgebot_muc{group_id = GroupId, muc_jid = MucJid, bot_name = BotName}).

run_test() ->
	run_test(ebridgebot_component_SUITE, []).
run_test(Suite) ->
	run_test(Suite, []).
run_test(Suite, Testcase) when is_atom(Testcase) ->
	run_test(Suite, [Testcase]);
run_test(Suite, Testcases) when is_atom(Suite) ->
	case application:get_env(ebridgebot, test_path, []) of
		[] -> ct:print("ERROR: test_path not found. Set ederibit_path in ejabberd section of sys.config", []),
			{error, invalid_deribit_path};
		DeribitPath ->
			ct:run_test(
				[{testcase, T} || T <- Testcases] ++
				[{suite, Suite},
					{dir, filename:join(DeribitPath, "test")},
					{include, ["_build/dev/lib/escalus", "include"]},
					{config, filename:join(DeribitPath, "test/test.config")}])
	end.