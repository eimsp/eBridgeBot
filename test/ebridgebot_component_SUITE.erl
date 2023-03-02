-module(ebridgebot_component_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

all() ->
	[{group, main}].

groups() ->
	MainStories = [test_msg, room_component_story],
	[{main, [sequence], MainStories}, {local, [sequence], MainStories}].

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
	Config2 = escalus:init_per_testcase(CaseName, Config),
	[_Host, RoomHost, Rooms, Users] =
		[escalus_ct:get_config(K) || K <- [ejabberd_domain, room_host, deribit_rooms, escalus_users]],
	[begin
		 Node = proplists:get_value(username, UserData),
		 Server = proplists:get_value(server, UserData),
		 mnesia:dirty_delete(deribit_storage, {Node, Server})
	 end || {_, UserData} <- Users],
	[begin
		 [Room, RoomOpts, Affs] = [proplists:get_value(K, Opts) || K <- [name, options, affiliations]],
		 catch mod_muc_admin:destroy_room(Room, RoomHost),
		 ok = mod_muc:create_room(RoomHost, Room, RoomOpts), %% TODO add affiliations in room options
		 timer:sleep(100),
		 [begin
			  UserCfg = proplists:get_value(U, Users),
			  Jid = jid:to_string(list_to_tuple([proplists:get_value(K, UserCfg) || K <- [username, server]] ++ [<<>>])),
			  ok = mod_muc_admin:set_room_affiliation(Room, RoomHost, Jid, atom_to_binary(Aff))
		  end || {U, Aff} <- Affs]
	 end || {_, Opts} <- Rooms],
	Config2.


end_per_testcase(test_msg, Config) ->
	meck:unload(ebridgebot_component),
	escalus:end_per_testcase(test_msg, Config);
end_per_testcase(CaseName, Config) ->
	escalus:end_per_testcase(CaseName, Config).

room_component_story(Config) ->
	RoomJid = <<"deribit_test@conference.localhost">>,
	[AliceNick] = [escalus_config:get_ct({escalus_users, U, nick}) || U <- [alice]],
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			ComponentJid = get_property(component, Config),
			CompNick = get_property(component, Config),
			Pid = get_property(component_pid, Config),
			enter_room(Alice, RoomJid, AliceNick),
			escalus_component:send(Pid, enter_groupchat(ComponentJid, <<"deribit_test@conference.localhost">>, CompNick)),
%%			escalus:send(Client, enter_groupchat(Client, RoomJid, Nick)),
			escalus_client:wait_for_stanzas(Alice, 2),
			ok
		end).

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



%% test API
get_property(PropName, Proplist) ->
	case lists:keyfind(PropName, 1, Proplist) of
		{PropName, Value} ->
			Value;
		false ->
			throw({missing_property, PropName})
	end.

enter_room(Client, RoomJid, Nick) ->
	escalus:send(Client, enter_groupchat(Client, RoomJid, Nick)),
	escalus_client:wait_for_stanzas(Client, 2). %% Client wait for 2 presences from ChatRoom

enter_groupchat(#client{jid = FromJid}, RoomJid, Nick) ->
	enter_groupchat(FromJid, RoomJid, Nick);
enter_groupchat(FromJid, RoomJid, Nick) ->
	#xmlel{name = <<"presence">>,
		attrs = [{<<"from">>, FromJid}, {<<"to">>, <<RoomJid/binary, "/", Nick/binary>>}],
		children = [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}]}]}.
%%%-------------------------------------------------------------------
%%% meck functions
%%%-------------------------------------------------------------------
process_stanza(Stanza, Client, State) ->
	[From, To] = [exml_query:attr(Stanza, X) || X <- [<<"from">>, <<"to">>]],
	Text = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
	EchoMsg = escalus_stanza:chat(To, From, Text),
	escalus:send(Client, EchoMsg),
	{ok, State}.
