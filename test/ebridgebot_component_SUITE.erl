-module(ebridgebot_component_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("xmpp/include/xmpp_codec.hrl").

all() ->
	[{group, main}].

groups() ->
	MainStories = [test_msg, room_component_story],
	[{main, [sequence], MainStories}, {local, [sequence], MainStories}].

init_per_suite(Config) ->
	[escalus:Fun([{escalus_user_db, {module, escalus_ejabberd}} | Config], escalus_users:get_users([alice])) || Fun <- [delete_users, create_users]],
	[{BotId, Pid, _, [escalus_component]} | _] = supervisor:which_children(ebridgebot_sup), %% get bot pid and name
	Bots = application:get_env(ebridgebot, bots, []),
	ConnectionArgs = get_property(BotId, Bots),
	ConnectionArgs ++ [{component_pid, Pid}, {bot_id, BotId} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
	escalus:end_per_suite(Config).

init_per_testcase(test_msg, Config) ->
	meck:new(ebridgebot_tg_component),
	meck:expect(ebridgebot_tg_component, process_stanza, fun process_stanza/3),
	escalus:init_per_testcase(test_msg, Config);
init_per_testcase(CaseName, Config) ->
	Config2 = escalus:init_per_testcase(CaseName, Config),
	[_Host, MucHost, Rooms, Users] =
		[escalus_ct:get_config(K) || K <- [ejabberd_domain, muc_host, ebridgebot_rooms, escalus_users]],
	[begin
		 Node = proplists:get_value(username, UserData),
		 Server = proplists:get_value(server, UserData),
		 mnesia:dirty_delete(deribit_storage, {Node, Server})
	 end || {_, UserData} <- Users],
	[begin
		 [Room, RoomOpts, Affs] = [proplists:get_value(K, Opts) || K <- [name, options, affiliations]],
		 catch mod_muc_admin:destroy_room(Room, MucHost),
		 ok = mod_muc:create_room(MucHost, Room, RoomOpts), %% TODO add affiliations in room options
		 timer:sleep(100),
		 ComponentJid = get_property(component, Config),
%%		 ok = mod_muc_admin:set_room_affiliation(Room, MucHost, ComponentJid, <<"admin">>),
		 [begin
			  UserCfg = proplists:get_value(U, Users),
			  Jid = jid:to_string(list_to_tuple([proplists:get_value(K, UserCfg) || K <- [username, server]] ++ [<<>>])),
			  ok = mod_muc_admin:set_room_affiliation(Room, MucHost, Jid, atom_to_binary(Aff))
		  end || {U, Aff} <- Affs]
	 end || {_, Opts} <- Rooms],
	Config2.


end_per_testcase(test_msg, Config) ->
	meck:unload(ebridgebot_tg_component),
	escalus:end_per_testcase(test_msg, Config);
end_per_testcase(CaseName, Config) ->
	catch meck:unload(ebridgebot_tg_component),
	escalus:end_per_testcase(CaseName, Config).

room_component_story(Config) ->
	RoomNode = escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, name}),
	MucHost = escalus_config:get_ct(muc_host),
	RoomJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	BotId = get_property(bot_id, Config),
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			[ComponentJid, ComponentNick, Pid] = [get_property(K, Config) || K <- [component, nick, component_pid]],
			enter_room(Alice, RoomJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 1),
			ebridgebot_tg_component:enter_groupchat(BotId, RoomJid),
			MucComponentJID = jid:make(RoomNode, MucHost, ComponentNick),
			#presence{from = MucComponentJID} = xmpp:decode(escalus:wait_for_stanza(Alice)),
			ComponentResp = <<"Hi, Alice!">>,
			Msg = #message{} =  xmpp:decode(escalus_stanza:groupchat_to(RoomJid, ComponentResp)),
			escalus_component:send(Pid, xmpp:encode(Msg#message{from = jid:make(<<>>, ComponentJid, <<>>)})),
			escalus:assert(is_groupchat_message, [ComponentResp], escalus:wait_for_stanza(Alice)),
			meck:new(ebridgebot_tg_component),
			meck:expect(ebridgebot_tg_component, process_stanza, fun process_stanza_response/3),
			AliceMsg = <<"Hi, bot!">>,
			escalus:send(Alice, escalus_stanza:groupchat_to(RoomJid, AliceMsg)),
			escalus:assert(is_groupchat_message, [AliceMsg], escalus:wait_for_stanza(Alice)),
			escalus:assert(is_groupchat_message, [<<"Response from bot">>], escalus:wait_for_stanza(Alice, 10000)),
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

process_stanza_response(#message{type = groupchat, from = From, to = To, body = [#text{data = <<"Hi, bot!">>}]} = Pkt, Client, State) ->
	escalus_connection:send(Client, xmpp:encode(Pkt#message{id = ebridgebot:gen_uuid(), from = jid:remove_resource(To), to = jid:remove_resource(From),
		body = [#text{data = <<"Response from bot">>}], sub_els = []})),
	{ok, State};
process_stanza_response(#xmlel{} = Stanza, Client, State) ->
	process_stanza_response(xmpp:decode(Stanza), Client, State);
process_stanza_response(_Stanza, _Client, State) ->
	{ok, State}.
