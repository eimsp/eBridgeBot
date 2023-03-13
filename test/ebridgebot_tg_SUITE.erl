-module(ebridgebot_tg_SUITE).
-author("cryoflamer").
-compile(export_all).
%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("xmpp/include/xmpp_codec.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-import(ebridgebot, [wait_for_result/2, wait_for_result/4, wait_for_list/1, wait_for_list/2]).

all() ->
	[{group, main}].

groups() ->
	MainStories = [muc_story, subscribe_muc_story],
	[{main, [sequence], MainStories}, {local, [sequence], MainStories}].

init_per_suite(Config) ->
	catch escalus:delete_users(Config),
	escalus:create_users(Config),
	application:stop(ebridgebot),
	escalus:init_per_suite(Config).

end_per_suite(Config) ->
	escalus:delete_users(Config),
	application:start(ebridgebot),
	escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
	[{BotId, BotArgs} | _] = escalus_ct:get_config(tg_bots),
	Args = [{bot_id, BotId},
		{component, escalus_ct:get_config(ejabberd_service)},
		{host, escalus_ct:get_config(ejabberd_addr)},
		{password, escalus_ct:get_config(ejabberd_service_password)},
		{port, escalus_ct:get_config(ejabberd_service_port)},
		{linked_rooms, []}] ++ BotArgs,
	{ok, Pid} = escalus_component:start({local, BotId}, ebridgebot_component, Args, Args),
	[_Host, MucHost, Rooms] =
		[escalus_ct:get_config(K) || K <- [ejabberd_domain, muc_host, ebridgebot_rooms]],
	[begin
		 [Room, ChatId] = [proplists:get_value(K, Opts) || K <- [name, chat_id]],

		 Pid ! {link_rooms, ChatId, jid:to_string({Room, MucHost, <<>>})},
		 #{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {out, unsubscribed}}]} = ebridgebot_component:state(Pid),

		 Pid ! enter_linked_rooms,
		 #{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {pending, unsubscribed}}]} = ebridgebot_component:state(Pid),
		 #{bot_id := BotId, rooms := [#muc_state{state = {in, _}}]} =
			 wait_for_result(fun() -> ebridgebot_component:state(Pid) end, %% wait for the room to be created and enter it
				 fun(#{rooms := [#muc_state{state = {in, _}}]}) -> true; (_) -> false end),
		 ok
	 end || {_, Opts} <- Rooms],
	[{component_pid, Pid} | Args ++ escalus:init_per_testcase(CaseName, Config)].


end_per_testcase(CaseName, Config) ->
	ok = ebridgebot_component:stop(get_property(component_pid, Config)),
	mnesia:delete_table(ebridgebot:bot_table(get_property(bot_id, Config))),
	escalus:end_per_testcase(CaseName, Config).

destroy_room(Config) ->
	[Pid, Component] = [get_property(K, Config) || K <- [component_pid, component]],
	[Rooms, MucHost] = [escalus_ct:get_config(K) || K <- [ebridgebot_rooms, muc_host]],
	[begin
		 RoomNode = get_property(name, Opts),
		 Iq = #iq{type = set,
			 from = jid:decode(Component), to = jid:make(RoomNode, MucHost),
			 sub_els = [#muc_owner{destroy = #muc_destroy{}}]},
		 escalus_component:send(Pid, xmpp:encode(Iq))
	 end || {_, Opts} <- Rooms].

muc_story(Config) ->
	[RoomNode, ChatId] = [escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, K}) || K <- [name, chat_id]],
	MucHost = escalus_config:get_ct(muc_host),
	RoomJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	[BotId, Pid, _Component, BotName] = [get_property(Key, Config) || Key <- [bot_id, component_pid, component, name]],
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			enter_room(Alice, RoomJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 2),

			AliceMsg = <<"Hi, bot!">>, AliceMsg2 = <<"Hi, bot! Edited">>,
			AlicePkt = xmpp:set_subtag(xmpp:decode(escalus_stanza:groupchat_to(RoomJid, AliceMsg)), #origin_id{id = OriginId = ebridgebot:gen_uuid()}),
			escalus:send(Alice, xmpp:encode(AlicePkt)),
			escalus:assert(is_groupchat_message, [AliceMsg], escalus:wait_for_stanza(Alice)),
			[_] = wait_for_list(fun() -> mnesia:dirty_all_keys(ebridgebot:bot_table(BotId)) end, 1),
			[#xmpp_link{xmpp_id = OriginId, uid = TgUid = #tg_id{}}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, OriginId, #xmpp_link.xmpp_id) end, 1),

			AlicePkt2 = #message{type = groupchat, to = RoomJID = jid:decode(RoomJid), body = [#text{data = AliceMsg2}], %% edit message from xmpp
				sub_els = [#replace{id = OriginId}, #origin_id{id = OriginId2 = ebridgebot:gen_uuid()}]},
			escalus:send(Alice, xmpp:encode(AlicePkt2)),
			escalus:assert(is_groupchat_message, [AliceMsg2], escalus:wait_for_stanza(Alice)),
			[#xmpp_link{xmpp_id = OriginId, uid = TgUid = #tg_id{chat_id = ChatId, id = MessageId}}, %% add edit link to bot link table
				#xmpp_link{xmpp_id = OriginId2, uid = TgUid = #tg_id{}}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, TgUid, #xmpp_link.uid) end, 2),

			AliceRetractPkt = #message{type = groupchat, to = RoomJID, %% retract from xmpp client
				sub_els = [#origin_id{id = ebridgebot:gen_uuid()}, #apply_to{id = OriginId2, sub_els = [#retract{}]}]},
			escalus:send(Alice, xmpp:encode(AliceRetractPkt)),
			#apply_to{id = OriginId2} = xmpp:get_subtag(xmpp:decode(escalus:wait_for_stanza(Alice)), #apply_to{}),
			[] = wait_for_list(fun() -> ebridgebot_component:index_read(BotId, TgUid, #xmpp_link.uid) end),
			[] = mnesia:dirty_all_keys(ebridgebot:bot_table(BotId)),

			TgAliceMsg = <<"Hello from telegram!">>, TgAliceMsg2 = <<"2: Hello from telegram!">>,
			Pid ! {pe4kin_update, BotName, tg_message(ChatId, MessageId + 1, AliceNick, TgAliceMsg)}, %% emulate sending message from Telegram
			escalus:assert(is_groupchat_message, [<<AliceNick/binary, ":\n", TgAliceMsg/binary>>], escalus:wait_for_stanza(Alice)),
			TgUid2 = TgUid#tg_id{id = MessageId +1},
			[#xmpp_link{uid = TgUid2}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, TgUid2, #xmpp_link.uid) end, 1),

			%% emulate editing message from Telegram
			Pid ! {pe4kin_update, BotName, tg_message(<<"edited_message">>, ChatId, MessageId + 1, AliceNick, TgAliceMsg2)},
			escalus:assert(is_groupchat_message, [<<AliceNick/binary, ":\n", TgAliceMsg2/binary>>], escalus:wait_for_stanza(Alice)),
			[#xmpp_link{uid = TgUid2}, #xmpp_link{uid = TgUid2}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, TgUid2, #xmpp_link.uid) end, 2),
			destroy_room(Config),
			escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),
			ok
		end).

subscribe_muc_story(Config) ->
	[RoomNode, ChatId] = [escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, K}) || K <- [name, chat_id]],
	MucHost = escalus_config:get_ct(muc_host),
	MucJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	[BotId, Pid, Component, BotName, Nick] = [get_property(Key, Config) || Key <- [bot_id, component_pid, component, name, nick]],
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			enter_room(Alice, MucJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 2),
			escalus_component:send(Pid, groupchat_presence(Component, MucJid, Nick, unavailable)),
			escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),
			Pid ! sub_linked_rooms,
			#{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {_, subscribed}}]} =
				wait_for_result(fun() -> ebridgebot_component:state(Pid) end,
					fun(#{rooms := [#muc_state{state = {_, subscribed}}]}) -> true; (_) -> false end),

			AliceMsg = <<"Hi, bot!">>, _AliceMsg2 = <<"Hi, bot! Edited">>,
			AlicePkt = xmpp:set_subtag(xmpp:decode(escalus_stanza:groupchat_to(MucJid, AliceMsg)), #origin_id{id = OriginId = ebridgebot:gen_uuid()}),
			escalus:send(Alice, xmpp:encode(AlicePkt)),
			escalus:assert(is_groupchat_message, [AliceMsg], escalus:wait_for_stanza(Alice)),
			[_] = wait_for_list(fun() -> mnesia:dirty_all_keys(ebridgebot:bot_table(BotId)) end, 1),
			[#xmpp_link{xmpp_id = OriginId, uid = TgUid = #tg_id{id = MessageId}}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, OriginId, #xmpp_link.xmpp_id) end, 1),

			TgAliceMsg = <<"Hello from telegram!">>,
			Pid ! {pe4kin_update, BotName, tg_message(ChatId, MessageId + 1, AliceNick, TgAliceMsg)}, %% emulate sending message from Telegram
			escalus:assert(is_groupchat_message, [<<AliceNick/binary, ":\n", TgAliceMsg/binary>>], escalus:wait_for_stanza(Alice)),
			TgUid2 = TgUid#tg_id{id = MessageId + 1},
			[#xmpp_link{uid = TgUid2}] =
				wait_for_list(fun() -> ebridgebot_component:index_read(BotId, TgUid2, #xmpp_link.uid) end, 1),
			destroy_room(Config),
			escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),
			ok
		end).

%% tg API
tg_message(ChatId, MessageId, Username, Text) ->
	tg_message(<<"message">>, ChatId, MessageId, Username, Text).
tg_message(Message, ChatId, MessageId, Username, Text) %% emulate Telegram message
	when Message == <<"message">>; Message == <<"edited_message">> ->
	#{Message =>
	#{<<"chat">> =>
	#{<<"id">> => ChatId, <<"title">> => <<"RoomTitle">>,
		<<"type">> => <<"group">>},
		<<"date">> => erlang:system_time(second),
		<<"from">> =>
		#{<<"first_name">> => Username,
			<<"id">> => rand:uniform(10000000000),
			<<"is_bot">> => false, %% TODO update if <<"is_bot">> == true
			<<"language_code">> => <<"en">>,
			<<"last_name">> => Username,
			<<"username">> => Username},
		<<"message_id">> => MessageId, <<"text">> => Text},
		<<"update_id">> => rand:uniform(10000000000)}.

%% test API
get_property(PropName, Proplist) ->
	case lists:keyfind(PropName, 1, Proplist) of
		{PropName, Value} ->
			Value;
		false ->
			throw({missing_property, PropName})
	end.

groupchat_presence(From, To, Nick) ->
	groupchat_presence(From, To, Nick, available).
groupchat_presence(#client{jid = From}, To, Nick, Type) ->
	groupchat_presence(From, To, Nick, Type);
groupchat_presence(From, To, Nick, Type) when is_binary(From), is_binary(To) ->
	xmpp:encode(#presence{type = Type, from = jid:make(From), to = jid:replace_resource(jid:decode(To), Nick), sub_els = [#muc{}]}).

enter_room(Client, RoomJid, Nick) ->
	escalus:send(Client, groupchat_presence(Client, RoomJid, Nick)),
	escalus_client:wait_for_stanzas(Client, 2). %% Client wait for 2 presences from ChatRoom