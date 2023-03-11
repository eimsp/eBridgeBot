-module(ebridgebot_tg).

%% API
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotName, BotToken] = [proplists:get_value(K, Args) || K <- [name, token]],
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
	{ok, #{token => BotToken}}.

handle_info({pe4kin_update, BotName,
	#{<<"message">> :=
	#{<<"chat">> := #{<<"type">> := <<"group">>, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}}} = TgMsg, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component} = State) ->
	ct:print("tg msg groupchat: ~p", [TgMsg]),
	[begin
		 OriginId = ebridgebot:gen_uuid(),
		 escalus:send(Client, xmpp:encode(#message{id = OriginId, type = groupchat, from = jid:decode(Component), to = jid:decode(MucJid),
			 body = [#text{data = <<TgUserName/binary, ":\n", Text/binary>>}], sub_els = [#origin_id{id = OriginId}]})),
		 ebridgebot_component:write_link(BotId, OriginId, #tg_id{chat_id = ChatId, id = Id})
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
		CurChatId == ChatId andalso (E == in orelse S == subscribed)], %% TODO maybe remove 'subscribed' state
	{ok, State};
handle_info({pe4kin_update, BotName,
	#{<<"edited_message">> :=
	#{<<"chat">> := #{<<"type">> := <<"group">>, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}}} = TgMsg, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component} = State) ->
	ct:print("edit tg msg groupchat: ~p", [TgMsg]),
	[case ebridgebot_component:index_read(BotId, Uid = #tg_id{chat_id = ChatId, id = Id}, #xmpp_link.uid) of
		 [#xmpp_link{xmpp_id = ReplaceId} | _] ->
			 Pkt = #message{id = OriginId} = ebridgebot_component:edit_msg(jid:decode(Component), jid:decode(MucJid), <<TgUserName/binary, ":\n", Text/binary>>, ReplaceId),
			 escalus:send(Client, xmpp:encode(Pkt)),
			 ebridgebot_component:write_link(BotId, OriginId, Uid); %% TODO maybe you don't need to write because there is no retract from Telegram
		 _ -> ok
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms, CurChatId == ChatId andalso (E == in orelse S == subscribed)],
	{ok, State};
handle_info({pe4kin_update, BotName, #{<<"message">> := _} = TgMsg}, _Client, #{bot_name := BotName} = State) ->
	ct:print("tg msg: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_update, BotName, TgMsg}, _Client, #{bot_name := BotName} = State) ->
	ct:print("tg msg2: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_send, ChatId, Text}, _Client, #{bot_name := BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	ct:print("pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info(Info, _Client, State) ->
	ct:print("handle component: ~p", [Info]),
	{ok, State}.

send_message(#{bot_name := BotName, chat_id := ChatId}, Text) ->
	case pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}) of
		{ok, #{<<"message_id">> := Id}} ->
			{ok, #tg_id{chat_id = ChatId, id = Id}};
		Err ->
			ct:print("ERROR: ~p", [Err]),
			Err
	end.