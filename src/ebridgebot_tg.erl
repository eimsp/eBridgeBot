-module(ebridgebot_tg).

%% API
-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-export([init/1, handle_info/3, send_message/2, edit_message/2, delete_message/1, get_file/1, link_pred/1]).

-define(CLEARING_INTERVAL, 24). %% in hours
-define(LIFE_SPAN, 48). %% in hours

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotName, BotToken] = [proplists:get_value(K, Args) || K <- [name, token]],
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),

	ClearingInterval = proplists:get_value(clearing_interval, Args, ?CLEARING_INTERVAL),
	LifeSpan = proplists:get_value(life_span, Args, ?LIFE_SPAN),
	self() ! {link_scheduler, ClearingInterval * 60 * 60 * 1000, LifeSpan * 60 * 60 * 1000}, %% start scheduler

	{ok, #{token => BotToken}}.

handle_info({pe4kin_update, BotName,
	#{<<"message">> :=
	#{<<"chat">> := #{<<"type">> := Type, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}}} = TgMsg, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component} = State) when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("tg msg groupchat: ~p", [TgMsg]),
	[begin
		 OriginId = ebridgebot:gen_uuid(),
		 escalus:send(Client, xmpp:encode(#message{id = OriginId, type = groupchat, from = jid:decode(Component), to = jid:decode(MucJid),
			 body = [#text{data = <<TgUserName/binary, ":\n", Text/binary>>}], sub_els = [#origin_id{id = OriginId}]})),
		 ebridgebot:write_link(BotId, OriginId, #tg_id{chat_id = ChatId, id = Id})
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
		CurChatId == ChatId andalso (E == in orelse S == subscribed)], %% TODO maybe remove 'subscribed' state
	{ok, State};
handle_info({pe4kin_update, BotName,
	#{<<"edited_message">> :=
	#{<<"chat">> := #{<<"type">> := Type, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}}} = TgMsg, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component} = State) when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("edit tg msg groupchat: ~p", [TgMsg]),
	[case ebridgebot:index_read(BotId, Uid = #tg_id{chat_id = ChatId, id = Id}, #xmpp_link.uid) of
		 [#xmpp_link{origin_id = ReplaceId} | _] ->
			 Pkt = #message{id = OriginId} = ebridgebot:edit_msg(jid:decode(Component), jid:decode(MucJid), <<TgUserName/binary, ":\n", Text/binary>>, ReplaceId),
			 escalus:send(Client, xmpp:encode(Pkt)),
			 ebridgebot:write_link(BotId, OriginId, Uid); %% TODO maybe you don't need to write because there is no retract from Telegram
		 _ -> ok
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms, CurChatId == ChatId andalso (E == in orelse S == subscribed)],
	{ok, State};
handle_info({pe4kin_update, BotName,
	#{<<"message">> :=
		#{<<"caption">> := Text,
		  <<"chat">> :=
				#{<<"id">> := CurChatId,
			      <<"type">> := Type},
		  <<"document">> :=
				#{<<"file_id">> := FileId,
				  <<"file_name">> := FileName,
				  <<"file_size">> := FileSize,
				  <<"file_unique_id">> := FileUniqueId,
				  <<"mime_type">> := ContentType},
		  <<"from">> :=
				#{<<"language_code">> := Lang,
				  <<"username">> := TgUserName},
		  <<"message_id">> := Id}} = TgMsg}, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component,
		token := Token, upload_host := UploadHost, upload := Upload} = State)
	when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("tg msg upload: ~p", [TgMsg]),
%%	{ok, #{<<"file_path">> := Path}} = pe4kin:get_file(BotName, #{file_id => FileId}),
%%	?dbg("tg file: ~p", [Path]),
%%	Uri = <<"/file/bot", Token/binary, "/", Path/binary>>,
%%	{200, _, Data} = pe4kin_http:get(Uri),
%%	?dbg("tg get: ~s", [Data]),

%%	SlotIq = #iq{id = <<FileId/binary, ";", (integer_to_binary(CurChatId))/binary, ";", TgUserName/binary>>, from = jid:decode(Component),
%%		type = get, to = jid:decode(UploadHost),
%%		sub_els = [#upload_request_0{filename = FileName, size = FileSize, 'content-type' = ContentType, xmlns = ?NS_HTTP_UPLOAD_0}]},
	SlotIq = #iq{id = FileId, type = get, from = jid:decode(Component), to = jid:decode(UploadHost),
		sub_els = [#upload_request_0{filename = FileName, size = FileSize, 'content-type' = ContentType, xmlns = ?NS_HTTP_UPLOAD_0}]},
	escalus:send(Client, xmpp:encode(SlotIq)),
	PutFun = fun(Data, PutURL) ->
		Res =
			httpc:request(put, {binary_to_list(PutURL), [], binary_to_list(ContentType), Data}, [], []),
		?dbg("put request: ~p", [Res])
%%		{ok, {{"HTTP/1.1", 201, _}, _, _}} =
%%			httpc:request(put, {binary_to_list(PutURL), [], binary_to_list(ContentType), Data}, [], [])
	end,
%%	[begin
%%		 IqId = ebridgebot:gen_uuid()
%%%%		 SlotIq = #iq{id = IqId, type = get, to = UploadJID,
%%%%			 sub_els = [#upload_request_0{filename = filename(), size = Size, 'content-type' = <<?CONTENT_TYPE>>, xmlns = ?NS_HTTP_UPLOAD_0}]},
%%%%		 escalus:send(Client, xmpp:encode(#message{id = OriginId, type = groupchat, from = jid:decode(Component), to = jid:decode(MucJid),
%%%%			 body = [#text{data = <<TgUserName/binary, ":\n", Text/binary>>}], sub_els = [#origin_id{id = OriginId}]})),
%%%%		 ebridgebot:write_link(BotId, OriginId, #tg_id{chat_id = ChatId, id = Id})
%%	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
%%		CurChatId == ChatId andalso (E == in orelse S == subscribed)], %% TODO maybe remove 'subscribed' state
	{ok, State#{upload => Upload#{FileId => PutFun}}};
handle_info({pe4kin_update, BotName, #{<<"message">> := _} = TgMsg}, _Client, #{bot_name := BotName} = State) ->
	?dbg("tg msg: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_update, BotName, TgMsg}, _Client, #{bot_name := BotName} = State) ->
	?dbg("tg msg2: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_send, ChatId, Text}, _Client, #{bot_name := BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	?dbg("pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info(Info, _Client, State) ->
	?dbg("handle component: ~p", [Info]),
	{ok, State}.

send_message(#{bot_name := BotName, chat_id := ChatId}, Text) ->
	case pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}) of
		{ok, #{<<"message_id">> := Id}} ->
			{ok, #tg_id{chat_id = ChatId, id = Id}};
		Err ->
			?dbg("ERROR: send_message: ~p", [Err]),
			Err
	end.

edit_message(#{bot_name := BotName, uid := #tg_id{chat_id = ChatId, id = Id}}, Text) ->
	case pe4kin:edit_message(BotName, #{chat_id => ChatId, message_id => Id, text => Text}) of
		{ok, _} ->
			{ok, #tg_id{chat_id = ChatId, id = Id}};
		Err ->
			?dbg("ERROR: : edit_message: ~p", [Err]),
			Err
	end.

delete_message(#{bot_name := BotName, uid := #tg_id{chat_id = ChatId, id = Id}}) ->
	case pe4kin:delete_message(BotName, #{chat_id => ChatId, message_id => Id}) of
		{ok, true} ->
			{ok, #tg_id{chat_id = ChatId, id = Id}};
		Err ->
			?dbg("ERROR: ~p", [Err]),
			Err
	end.

get_file(#{file_id := FileId, bot_name := BotName, token := Token}) ->
	{ok, #{<<"file_path">> := FilePath}} = pe4kin:get_file(BotName, #{file_id => FileId}),
	case pe4kin_http:get(<<"/file/bot", Token/binary, "/", FilePath/binary>>) of
		{200, _, Data} -> {ok, Data};
		{_ErrCode, _, _Reason} = Err ->
			?dbg("ERROR: ~p", [Err]),
			{error, get_file}
	end.

link_pred(#{group_id := ChatId}) -> %% filter link predicate
	fun(#xmpp_link{uid = #tg_id{chat_id = ChatId2}}) when  ChatId == ChatId2 -> true;
		(_Link) -> false
	end.
