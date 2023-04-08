-module(ebridgebot_tg).

%% API
-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-export([init/1, handle_info/3, send_message/1, edit_message/1, delete_message/1, send_data/1, get_file/1, link_pred/1]).

-define(CLEARING_INTERVAL, 24). %% in hours
-define(LIFE_SPAN, 48). %% in hours

init(Args) ->
	?dbg("init pe4kin with args: ~p", [Args]),
	application:ensure_all_started(pe4kin),
	[BotName, BotToken] = [proplists:get_value(K, Args) || K <- [name, token]],
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),

	ClearingInterval = proplists:get_value(clearing_interval, Args, ?CLEARING_INTERVAL),
	LifeSpan = proplists:get_value(life_span, Args, ?LIFE_SPAN),
	IgnoreCommands = proplists:get_value(ignore_commands, Args, true),
	self() ! {link_scheduler, ClearingInterval * 60 * 60 * 1000, LifeSpan * 60 * 60 * 1000}, %% start scheduler

	{ok, #{bot_name => BotName, token => BotToken, ignore_commands => IgnoreCommands}}.

handle_info({telegram_update, BotName, _SendType,
		#{<<"chat">>        := #{<<"type">> := Type},
		  <<"entities">>    := [#{<<"offset">> := 0, <<"type">> := <<"bot_command">>} | _]} = TgMsg}, _Client,
	#{bot_name := BotName, ignore_commands := true} = State) when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("ignore commands from tg: ~p", [TgMsg]),
	{ok, State};
handle_info({telegram_update, BotName, SendType,
		#{<<"chat">>        := #{<<"id">> := ChatId, <<"type">> := Type},
		 <<"document">>     := #{<<"file_id">> := FileId},
		 <<"from">>         := #{<<"username">> := TgUserName},
		 <<"message_id">>   := Id} = TgMsg}, Client,
	#{bot_name := BotName, rooms := Rooms, component := Component, upload_host := UploadHost, upload := Upload} = State)
	when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("telegram_update: upload: ~p", [TgMsg]),
	Text = case maps:find(<<"caption">>, TgMsg) of {ok, V} -> <<V/binary, $\n>>; _ -> <<>> end,
	case ebridgebot:to_rooms(ChatId, Rooms, fun(_, MucJid) -> MucJid end) of
		[] -> {ok, State};
		MucJids ->
			{ok, #{<<"file_path">> := FilePath, <<"file_size">> := FileSize}} = pe4kin:get_file(BotName, #{file_id => FileId}),
			FileName = filename:basename(FilePath),
			SlotIq = #iq{id = FileId, type = get, from = jid:decode(Component), to = jid:decode(UploadHost),
				sub_els = [#upload_request_0{filename = FileName, size = FileSize,
					'content-type' = ContentType = hd(mimetypes:filename(FilePath)), xmlns = ?NS_HTTP_UPLOAD_0}]},
			escalus:send(Client, xmpp:encode(SlotIq)), %% send slot iq
			{ok, State#{upload =>
				Upload#{FileId =>
					#upload_info{file_id = FileId,
						caption = Text,
						content_type = ContentType,
						nick = TgUserName,
						file_path = FilePath,
						muc_jids = MucJids,
						uid = #tg_id{chat_id = ChatId, id = Id},
						send_type = SendType}}}}
	end;
handle_info({telegram_update, BotName, SendType,
	#{<<"reply_to_message">> :=
		#{<<"from">> :=
			#{<<"username">> := QuotedUser},
			  <<"text">> := QuotedText},
	 <<"text">> := Text} = TgMsg}, Client, State) ->
	?dbg("telegram_update: reply_to_message: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"reply_to_message">>, TgMsg),
	Text2 = binary:replace(QuotedText, <<"\n">>, <<">">>, [global, {insert_replaced, 0}]),
	Text3 = <<$>, QuotedUser/binary, "\n>", Text2/binary, $\n, Text/binary>>,
	handle_info({telegram_update, BotName, SendType, TgMsg2#{<<"text">> := Text3}}, Client, State);
handle_info({telegram_update, BotName, SendType,
		#{<<"forward_from">> := #{<<"username">> := QuotedUser}, <<"text">> := Text} = TgMsg}, Client, State) ->
	?dbg("telegram_update: forward_from: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"forward_from">>, TgMsg),
	Text2 = <<$>, QuotedUser/binary, ":\n", Text/binary>>,
	handle_info({telegram_update, BotName, SendType, TgMsg2#{<<"text">> := Text2}}, Client, State);
handle_info({telegram_update, BotName, SendType, #{<<"sticker">> := #{<<"emoji">> := Emoji}} = TgMsg}, Client, State) ->
	?dbg("telegram_update: sticker: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"sticker">>, TgMsg),
	handle_info({telegram_update, BotName, SendType, TgMsg2#{<<"text">> => Emoji}}, Client, State);
handle_info({telegram_update, BotName, SendType, #{<<"sticker">> := Sticker} = TgMsg}, Client, State) -> %% if sticker is not linked with emoji
	?dbg("telegram_update: sticker without emoji: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"sticker">>, TgMsg),
	handle_info({telegram_update, BotName, SendType, TgMsg2#{<<"document">> => Sticker}}, Client, State);
handle_info({telegram_update, BotName, SendType, TgMsg}, Client, State)
	when is_map_key(<<"photo">>, TgMsg); is_map_key(<<"video">>, TgMsg); is_map_key(<<"audio">>, TgMsg); is_map_key(<<"voice">>, TgMsg) ->
	?dbg("telegram_update: photo | video | audio | voice, ~p, ~p", [SendType, TgMsg]),
	ReplaceFun =
		fun ReplaceFun([], Map) -> Map;
			ReplaceFun([Key | T], Map) ->
				case maps:take(Key, Map) of
					{[Doc | _], Map2} -> Map2#{<<"document">> => Doc};
					{Doc, Map2} -> Map2#{<<"document">> => Doc};
					_ ->
						ReplaceFun(T, Map)
				end
		end,
	TgMsg2 = ReplaceFun([<<"photo">>, <<"video">>, <<"audio">>, <<"voice">>], TgMsg),
	handle_info({telegram_update, BotName, SendType, TgMsg2}, Client, State);
handle_info({telegram_update, BotName, SendType,
	#{<<"chat">> := #{<<"type">> := Type, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}} = TgMsg, Client,
	#{bot_id := BotId, bot_name := BotName, rooms := Rooms, component := Component} = State) when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("telegram_update: msg to groupchat: ~p", [TgMsg]),
	ebridgebot:to_rooms(CurChatId, Rooms,
		fun(ChatId, MucJid) ->
			ebridgebot:send(SendType, Client, BotId, Component, MucJid, #tg_id{chat_id = ChatId, id = Id}, TgUserName, Text)
		end),
	{ok, State};
handle_info({pe4kin_update, BotName, #{<<"message">> := TgMsg}}, Client, State) ->
	handle_info({telegram_update, BotName, msg, TgMsg}, Client, State);
handle_info({pe4kin_update, BotName, #{<<"edited_message">> := TgMsg}}, Client, State) ->
	handle_info({telegram_update, BotName, edit_msg, TgMsg}, Client, State);
handle_info({pe4kin_update, BotName, TgMsg}, _Client, #{bot_name := BotName} = State) ->
	?dbg("pe4kin_update: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_send, ChatId, Text}, _Client, #{bot_name := BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	?dbg("pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info(Info, _Client, State) ->
	?dbg("handle component: ~p", [Info]),
	{ok, State}.


-spec send_message(#{bot_name => atom(), chat_id => integer(), text => binary(), usernick => binary(), format => #{} | #{usernick => atom() | binary()}}) ->
	{ok, #tg_id{}} | {error, atom(), term()}.
send_message(#{bot_name := BotName, chat_id := ChatId, text := Text, usernick := Nick, format := Format}) ->
	Msg = nick_format(Nick, Text, Format),
	format(pe4kin:send_message(BotName, Msg#{chat_id => ChatId})).

-spec edit_message(#{bot_name => atom(), uid => #tg_id{}, text => binary(), usernick => binary(), format => #{} | #{usernick => atom() | binary()}}) ->
	{ok, #tg_id{}} | {error, atom(), term()}.
edit_message(#{bot_name := BotName, uid := #tg_id{chat_id = ChatId, id = Id} = TgId, text := Text, usernick := Nick, format := Format}) ->
	Msg = nick_format(Nick, Text, Format),
	case pe4kin:edit_message(BotName, Msg#{chat_id => ChatId, message_id => Id}) of
		{ok, _} -> {ok, TgId};
		Err -> ?err("ERROR: edit_message: ~p", [Err]), Err
	end.

delete_message(#{bot_name := BotName, uid := #tg_id{chat_id = ChatId, id = Id} = TgId}) ->
	case pe4kin:delete_message(BotName, #{chat_id => ChatId, message_id => Id}) of
		{ok, true} -> {ok, TgId};
		Err -> ?err("ERROR: delete_message: ~p", [Err]), Err
	end.

get_file(#{token := Token, file_path := FilePath}) ->
	case pe4kin_http:get(<<"/file/bot", Token/binary, "/", FilePath/binary>>) of
		{200, _, Data} -> {ok, Data};
		{_ErrCode, _, _Reason} = Err ->
			?err("ERROR: ~p", [Err]),
			{error, invalid_get_file}
	end.

send_data(#{bot_name := Bot, mime := Mime, chat_id := ChatId, file_uri := FileUri, caption := Caption}) ->
	{UploadFun, UploadKey} =
		case Mime of
			<<"image/", _/binary>> -> {send_photo, photo};
			<<"audio/", _/binary>> -> {send_audio, audio};
			<<"video/", _/binary>> -> {send_video, video};
			_ -> {send_document, document}
		end,
	format(pe4kin:UploadFun(Bot, #{chat_id => ChatId, UploadKey => FileUri, caption => Caption})).

format({ok, #{<<"message_id">> := MessageId, <<"chat">> := #{<<"id">> := ChatId}}}) ->
	{ok, #tg_id{chat_id = ChatId, id = MessageId}};
format({ok, #{<<"result">> := Result}}) ->
	format({ok, Result});
format(Err) ->
	?err("ERROR: send_message: ~p", [Err]), Err.

link_pred(#{group_id := ChatId}) -> %% filter link predicate
	fun(#xmpp_link{uid = #tg_id{chat_id = ChatId2}}) when  ChatId == ChatId2 -> true;
		(_Link) -> false
	end.

-spec nick_format(binary(), binary(), map()) -> map().
nick_format(Nick, Text, Format) ->
	Entities = case Format of #{usernick := Type} -> #{entities => [#{offset => 0, length => size(Nick), type => Type}]}; _ -> #{} end,
	Entities#{text => <<Nick/binary, ":\n", Text/binary>>}.