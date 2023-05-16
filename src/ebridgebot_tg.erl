-module(ebridgebot_tg).

%% API
-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-export([init/1, handle_info/3, send_message/1, edit_message/1, delete_message/1, send_data/1, get_file/1, link_pred/1]).

-define(CLEARING_INTERVAL, 24). %% in hours
-define(LIFE_SPAN, 48). %% in hours

-record(telegram_update, {msg = #{}                         :: map(),
						  packet_fun = ebridgebot:pkt_fun() :: function()}).

init(#{bot_name := BotName, token := Token} = Args) ->
	?dbg("ebridgebot_tg: init pe4kin with args: ~p", [Args]),
	application:ensure_all_started(pe4kin),
	pe4kin:launch_bot(BotName, Token, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),

	DefaultState = #{clearing_interval => ?CLEARING_INTERVAL, life_span => ?LIFE_SPAN, ignore_commands => true},
	State = #{clearing_interval := ClearingInterval, life_span := LifeSpan} = maps:merge(DefaultState, Args),
	self() ! {link_scheduler, ClearingInterval * 60 * 60 * 1000, LifeSpan * 60 * 60 * 1000}, %% start scheduler

	{ok, State}.

handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"edited_message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"message_id">> := MessageId} = TgMsg} = EditMsg},
	Client, #{bot_id := BotId} = State) ->
	?dbg("handle component: edit: ~p", [TgMsg]),
	NewPktFun =
		case ebridgebot:index_read(BotId, #tg_id{chat_id = ChatId, id = MessageId}, #xmpp_link.uid) of
			[#xmpp_link{origin_id = ReplaceId} | _] ->
				ebridgebot:pkt_fun(tag, PktFun, #replace{id = ReplaceId});
			_ -> PktFun
		end,
	handle_info(#telegram_update{packet_fun = NewPktFun,
								 msg = maps:remove(<<"edited_message">>, EditMsg#{<<"message">> => TgMsg})}, Client, State);
handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"message">> := #{<<"from">> := #{<<"username">> := TgUserName, <<"language_code">> := Lang}} = TgMsg}},
		Client, #{component := ComponentJid} = State) ->
	?dbg("handle component: message: ~p", [TgMsg]),
	PktFun2 = ebridgebot:fold_pkt_fun([{lang, Lang}, {from, ComponentJid}, {text, <<?NICK(TgUserName)>>}], PktFun),
	handle_info(#telegram_update{msg = TgMsg, packet_fun = PktFun2}, Client, State);
handle_info(#telegram_update{msg = #{<<"entities">> := [#{<<"offset">> := 0, <<"type">> := <<"bot_command">>} | _]} = TgMsg}, _Client,
	#{ignore_commands := true} = State) ->
	?dbg("handle component: ignore commands from tg: ~p", [TgMsg]),
	{ok, State};
handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"chat">> := #{<<"type">> := Type} = Chat} = TgMsg} = TgUpd, Client, State)
	when Type == <<"group">>; Type == <<"supergroup">> ->
	?dbg("telegram type only group or supergropu: ~p", [TgMsg]),
	handle_info(TgUpd#telegram_update{packet_fun = ebridgebot:pkt_fun(type, PktFun, groupchat),
									  msg = TgMsg#{<<"chat">> => maps:remove(<<"type">>, Chat)}}, Client, State);
handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"entities">> := Entities, <<"from">> := #{<<"username">> := TgUserName}} = TgMsg}, Client, State) ->
	?dbg("handle component: entities: ~p", [TgMsg]),
	NickOffset = byte_size(<<?NICK(TgUserName)>>),
	Es = [#message_entity{offset = NickOffset + Offset, length = Length, type = binary_to_atom(Type)} ||
		#{<<"offset">> := Offset, <<"length">> := Length, <<"type">> := Type} <- Entities],
	PktFun2 = ebridgebot:pkt_fun(tag, PktFun, #message_entities{items = Es}),
	handle_info(#telegram_update{msg = maps:remove(<<"entities">>, TgMsg), packet_fun = PktFun2}, Client, State);
handle_info(#telegram_update{msg = #{<<"chat">> := #{<<"type">> := Type}} = TgMsg}, _Client, State) ->
	?dbg("ignore ~s telegram type for\n~p", [Type, TgMsg]),
	%% TODO not implemented
	{ok, State};
handle_info(#telegram_update{msg = #{<<"chat">> := #{<<"id">> := _ChatId}, <<"caption">> := Caption} = TgMsg, packet_fun = PktFun},
	Client,	State) ->
	?dbg("telegram_update: caption: ~p", [TgMsg]),
	PktFun2 = ebridgebot:pkt_fun(text, PktFun, <<Caption/binary, $\n>>),
	handle_info(#telegram_update{msg = maps:remove(<<"caption">>, TgMsg), packet_fun = PktFun2}, Client, State);
handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"chat">> := #{<<"id">> := ChatId},
									 <<"document">> := #{<<"file_id">> := FileId},
									 <<"message_id">> := Id} = TgMsg}, Client,
	#{bot_name := BotName, rooms := Rooms, component := Component, upload_host := UploadHost, upload := Upload} = State) ->
	?dbg("telegram_update: upload: ~p", [TgMsg]),
	case ebridgebot:to_rooms(ChatId, Rooms) of
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
						content_type = ContentType,
						file_path = FilePath,
						muc_jids = MucJids,
						uid = #tg_id{chat_id = ChatId, id = Id},
						packet_fun = PktFun}}}}
	end;
handle_info(#telegram_update{packet_fun = PktFun,
							 msg = #{<<"chat">> := #{<<"id">> := ChatId},
								    <<"from">> := #{<<"username">> := TgUsername},
								    <<"reply_to_message">> :=
										#{<<"from">> := #{<<"username">> := QuotedUser},
										  <<"message_id">> := Id,
										  <<"text">> := QuotedText}} = TgMsg},
	Client, #{bot_id := BotId, nick := Nick} = State) ->
	?dbg("telegram_update: reply_to_message: ~p", [TgMsg]),
	Text2 = binary:replace(QuotedText, <<"\n">>, <<">">>, [global, {insert_replaced, 0}]),
	RepliedText = <<$>, QuotedUser/binary, "\n>", Text2/binary>>,
	Text3 = <<RepliedText/binary, $\n>>,
	NickSize = byte_size(<<?NICK(TgUsername)>>),
	PktFun2 =
		case ebridgebot:index_read(BotId, #tg_id{chat_id = ChatId, id = Id}, #xmpp_link.uid) of
			[#xmpp_link{origin_id = OriginId} | _] -> %% TODO solve many to many problem!
				fun(Pkt, To) ->
					ReplyTag = #reply{id = OriginId, to = jid:replace_resource(jid:decode(To), Nick)},
					FallbackTag = #fallback{for = ?NS_REPLY, body = [#fb_body{start = NickSize, 'end' = NickSize + byte_size(RepliedText)}]},
					(ebridgebot:fold_pkt_fun([{tag, [ReplyTag, FallbackTag]}, {text, Text3}], PktFun))(Pkt, To)
				end;
			[] -> PktFun
		end,
	handle_info(#telegram_update{msg = maps:remove(<<"reply_to_message">>, TgMsg), packet_fun = PktFun2}, Client, State);
handle_info(#telegram_update{msg = #{<<"forward_from">> := #{<<"username">> := QuotedUser}, <<"text">> := Text} = TgMsg} = TgUpd, Client, State) ->
	?dbg("telegram_update: forward_from: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"forward_from">>, TgMsg),
	Text2 = <<$>, QuotedUser/binary, ":\n", Text/binary>>,
	handle_info(TgUpd#telegram_update{msg = TgMsg2#{<<"text">> := Text2}}, Client, State);
handle_info(#telegram_update{msg = #{<<"sticker">> := #{<<"emoji">> := Emoji}} = TgMsg} = TgUpd, Client, State) ->
	?dbg("telegram_update: sticker: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"sticker">>, TgMsg),
	handle_info(TgUpd#telegram_update{msg = TgMsg2#{<<"text">> => Emoji}}, Client, State);
handle_info(#telegram_update{msg = #{<<"sticker">> := Sticker} = TgMsg} = TgUpd, Client, State) -> %% if sticker is not linked with emoji
	?dbg("telegram_update: sticker without emoji: ~p", [TgMsg]),
	TgMsg2 = maps:remove(<<"sticker">>, TgMsg),
	handle_info(TgUpd#telegram_update{msg = TgMsg2#{<<"document">> => Sticker}}, Client, State);
handle_info(#telegram_update{msg = #{<<"chat">> := #{<<"id">> := _ChatId}, <<"text">> := Text} = TgMsg, packet_fun = PktFun},
	Client,	State) ->
	?dbg("telegram_update: text: ~p", [TgMsg]),
	PktFun2 = ebridgebot:pkt_fun(text, PktFun, Text),
	handle_info(#telegram_update{msg = maps:remove(<<"text">>, TgMsg), packet_fun = PktFun2}, Client, State);
handle_info(#telegram_update{msg = TgMsg} = TgUpd, Client, State)
	when is_map_key(<<"photo">>, TgMsg); is_map_key(<<"video">>, TgMsg); is_map_key(<<"audio">>, TgMsg); is_map_key(<<"voice">>, TgMsg) ->
	?dbg("telegram_update: photo | video | audio | voice, ~p", [TgMsg]),
	ReplaceFun =
		fun ReplaceFun([], Map) -> Map;
			ReplaceFun([Key | T], Map) ->
				case maps:take(Key, Map) of
					{[Doc | _], Map2} -> Map2#{<<"document">> => Doc};
					{Doc, Map2} -> Map2#{<<"document">> => Doc};
					_ -> ReplaceFun(T, Map)
				end
		end,
	TgMsg2 = ReplaceFun([<<"photo">>, <<"video">>, <<"audio">>, <<"voice">>], TgMsg),
	handle_info(TgUpd#telegram_update{msg = TgMsg2}, Client, State);
handle_info(#telegram_update{msg = #{<<"chat">> := #{<<"id">> := ChatId}, <<"message_id">> := Id}, packet_fun = PktFun} = TgMsg,
			Client,	#{bot_id := BotId, rooms := Rooms} = State) ->
	?dbg("telegram_update: msg to groupchat: ~p\n~p", [TgMsg, State]),
	ebridgebot:to_rooms(ChatId, Rooms,
		fun(MucJid) -> ebridgebot:send_to(Client, PktFun, MucJid, BotId, #tg_id{chat_id = ChatId, id = Id}) end),
	{ok, State};
handle_info({pe4kin_update, BotName, #{} = TgMsg}, Client, #{bot_name := BotName} = State) ->
	handle_info(#telegram_update{msg = TgMsg}, Client, State);
handle_info({pe4kin_send, ChatId, Text}, _Client, #{bot_name := BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	?dbg("pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info(Info, _Client, State) ->
	?dbg("handle component: ~p\n~p", [Info, State]),
	{ok, State}.


-spec send_message(#{bot_name => atom(), chat_id => integer(), text => binary(), usernick => binary(), format => #{} | #{usernick => atom() | binary()}}) ->
	{ok, #tg_id{}} | {error, atom(), term()}.
send_message(#{bot_name := BotName, chat_id := ChatId} = State) ->
	Msg = msg_reply(msg_format(State), State),
	format(pe4kin:send_message(BotName, Msg#{chat_id => ChatId})).

-spec edit_message(#{bot_name => atom(), uid => #tg_id{}, text => binary(), usernick => binary(), format => #{} | #{usernick => atom() | binary()}}) ->
	{ok, #tg_id{}} | {error, atom(), term()}.
edit_message(#{bot_name := BotName, uid := #tg_id{chat_id = ChatId, id = Id} = TgId} = State) ->
	Msg = msg_reply(msg_format(State), State),
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

-spec msg_format(map()) -> map().
msg_format(#{text := <<">", _/binary>> = Text, format := #{reply := ReplyType} = Format, entities := []} = State) when not is_map_key(reply_to, State) ->
	msg_format(State#{format => maps:remove(reply, Format), entities => [#{offset => 0, length => reply_offset(Text), type => ReplyType}]});
msg_format(#{text := Text, usernick := Nick, format := Format, entities := Entities}) when is_list(Entities) ->
	Nick2 = <<?NICK(Nick)>>,
	Es2 =
		case Format of
			#{usernick := Type} -> [#{offset => 0, length => byte_size(Nick2), type => Type}];
			_ -> []
		end,
	#{entities => Es2 ++ [Entity#{offset => byte_size(Nick2) + Offset} || #{offset := Offset} = Entity <- Entities], text => <<Nick2/binary, Text/binary>>};

msg_format(#{text := Text, usernick := Nick, format := Format}) ->
	{_, Es} =
		lists:foldl(
			fun({Key, Val}, {Offset, Acc}) ->
				Len = size(Val),
				{Offset + Len, case Format of #{Key := Type} -> [#{offset => Offset, length => Len, type => Type} | Acc]; _ -> Acc end}
			end, {0, []}, [{usernick, Nick2 = <<?NICK(Nick)>>}, {text, Text}]),
	#{entities => Es, text => <<Nick2/binary, Text/binary>>}.

msg_reply(#{} = Msg, #{chat_id := ChatId, reply_to := #tg_id{chat_id = ChatId, id = ReplyToId}}) ->
	Msg#{reply_to_message_id => ReplyToId};
msg_reply(Msg, _State) ->
	Msg.

reply_offset(Text) ->
	reply_offset(Text, {$\n, 0}).
reply_offset(<<>>, {_, N}) ->
	N;
reply_offset(<<$>, Text/binary>>, {$\n, N}) ->
	reply_offset(Text, {$>, N + 1});
reply_offset(<<$\n, Text/binary>>, {$>, N}) ->
	reply_offset(Text, {$\n, N + 1});
reply_offset(<<_/integer, Text/binary>>, {$>, N}) ->
	reply_offset(Text, {$>, N + 1});
reply_offset(<<_/integer, _Text/binary>>, {$\n, N}) ->
	N.
