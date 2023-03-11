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

-import(ebridgebot_component_SUITE, [get_property/2, enter_room/3]).
-import(ebridgebot, [wait_for_result/2, wait_for_result/4, wait_for_list/1, wait_for_list/2]).

all() ->
	[{group, main}].

groups() ->
	MainStories = [muc_story, subscribe_muc_story],
	[{main, [sequence], MainStories}, {local, [sequence], MainStories}].

init_per_suite(Config) ->
	[escalus:Fun([{escalus_user_db, {module, escalus_ejabberd}} | Config], escalus_users:get_users([alice])) || Fun <- [delete_users, create_users]],
	application:stop(ebridgebot),
	escalus:init_per_suite(Config).

end_per_suite(Config) ->
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
	ebridgebot_component_SUITE:init_per_testcase(CaseName, [{component_pid, Pid} | Args ++ Config]).


end_per_testcase(CaseName, Config) ->
	BotId = get_property(bot_id, Config),
	ok = ebridgebot_component:stop(get_property(component_pid, Config)),
	{ok, atomic} = mnesia:delete_table(ebridgebot:bot_table(BotId)),
	escalus:end_per_testcase(CaseName, Config).

muc_story(Config) ->
	[RoomNode, ChatId] = [escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, K}) || K <- [name, chat_id]],
	MucHost = escalus_config:get_ct(muc_host),
	RoomJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	[BotId, Pid, _Component, BotName] = [get_property(Key, Config) || Key <- [bot_id, component_pid, component, name]],
	#{bot_id := BotId, rooms := []} = ebridgebot_component:state(Pid),
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			enter_room(Alice, RoomJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 1),
			Pid ! {link_rooms, ChatId, RoomJid},
			#{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {out, unsubscribed}}]} = ebridgebot_component:state(Pid),

			Pid ! enter_linked_rooms,
			#{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {pending, unsubscribed}}]} = ebridgebot_component:state(Pid),
			escalus_client:wait_for_stanzas(Alice, 1),

			#{bot_id := BotId, rooms := [#muc_state{state = {in, _}}]} =
				wait_for_result(fun() -> ebridgebot_component:state(Pid) end,
					fun(#{rooms := [#muc_state{state = {in, _}}]}) -> true; (_) -> false end),

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
			ok
		end).

subscribe_muc_story(Config) ->
	[RoomNode, ChatId] = [escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, K}) || K <- [name, chat_id]],
	MucHost = escalus_config:get_ct(muc_host),
	RoomJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	[BotId, Pid, _Component, BotName] = [get_property(Key, Config) || Key <- [bot_id, component_pid, component, name]],
	#{bot_id := BotId, rooms := []} = ebridgebot_component:state(Pid),
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			enter_room(Alice, RoomJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 1),

			Pid ! {link_rooms, ChatId, RoomJid},
			#{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {out, unsubscribed}}]} = ebridgebot_component:state(Pid),

			Pid ! sub_linked_rooms,
			#{bot_id := BotId, rooms := [#muc_state{group_id = ChatId, state = {out, subscribed}}]} =
				wait_for_result(fun() -> ebridgebot_component:state(Pid) end,
					fun(#{rooms := [#muc_state{state = {_, subscribed}}]}) -> true; (_) -> false end),

			AliceMsg = <<"Hi, bot!">>, _AliceMsg2 = <<"Hi, bot! Edited">>,
			AlicePkt = xmpp:set_subtag(xmpp:decode(escalus_stanza:groupchat_to(RoomJid, AliceMsg)), #origin_id{id = OriginId = ebridgebot:gen_uuid()}),
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