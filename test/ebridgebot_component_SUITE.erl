-module(ebridgebot_component_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").

all() ->
	[{group, main}].

groups() ->
	[{main, [sequence], [test_msg]}, {local, [sequence], [test_msg]}].

init_per_suite(Config) ->
	[escalus:Fun([{escalus_user_db, {module, escalus_ejabberd}} | Config], escalus_users:get_users([alice])) || Fun <- [delete_users, create_users]],
	[{BotName, Pid, _, [escalus_component]} | _] = supervisor:which_children(ebridgebot_sup),
	Bots = application:get_env(ebridgebot, bots, []),
	ConnectionArgs = get_property(BotName, Bots),
	ConnectionArgs ++ [{component_pid, Pid}, {bot_name, BotName} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
	escalus:end_per_suite(Config).

init_per_testcase(test_msg, Config) ->
	meck:new(ebridgebot_component),
	meck:expect(ebridgebot_component, process_stanza, fun process_stanza/3),
	escalus:init_per_testcase(test_msg, Config);
init_per_testcase(CaseName, Config) ->
	escalus:init_per_testcase(CaseName, Config).

end_per_testcase(test_msg, Config) ->
	meck:unload(ebridgebot_component),
	escalus:end_per_testcase(test_msg, Config);
end_per_testcase(CaseName, Config) ->
	escalus:end_per_testcase(CaseName, Config).



test_msg(Config) ->
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			ComponentJid = get_property(component, Config),
			[begin
				 Msg = escalus_stanza:chat_to(ComponentJid, Text),
				 escalus:send(Alice, Msg),
				 escalus:assert(is_chat_message_from_to,
					 [ComponentJid,
						 escalus_client:full_jid(Alice),
						 Text],
					 escalus:wait_for_stanza(Alice))
			 end || Text <- [<<"Hi!">>, <<"I'm">>, <<"Alice">>]],
			ok
		end).

get_property(PropName, Proplist) ->
	case lists:keyfind(PropName, 1, Proplist) of
		{PropName, Value} ->
			Value;
		false ->
			throw({missing_property, PropName})
	end.

%%%-------------------------------------------------------------------
%%% meck functions
%%%-------------------------------------------------------------------
process_stanza(Stanza, Client, State) ->
	[From, To] = [exml_query:attr(Stanza, X) || X <- [<<"from">>, <<"to">>]],
	Text = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
	EchoMsg = escalus_stanza:chat(To, From, Text),
	escalus:send(Client, EchoMsg),
	{ok, State}.
