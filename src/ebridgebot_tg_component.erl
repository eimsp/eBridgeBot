-module(ebridgebot_tg_component).
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").

-export([init/1, handle_info/3, process_stanza/3, terminate/2]).

-record(tg_state, {
	bot_id = [] :: atom(),
	bot_pid = [] :: pid(),
	bot_name = [] :: binary(),
	component = [] :: binary(),
	nick = [] :: binary(),
	token = [] :: binary(),
	rooms = [] :: list(),
	context = [] :: any()}).

-record(muc_state, {group_id = [] :: integer(), muc_jid = [] :: binary(), state = out :: out | in | pending}).

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotId, BotName, BotToken, Component, Nick, Token, Rooms] =
		[proplists:get_value(K, Args) ||
		 K <- [bot_id, name, token, component, nick, token, linked_rooms]], %% TODO unused params need remove in future
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit=>100, timeout=>60}),
	NewRooms = [#muc_state{group_id = TgId, muc_jid = MucJid} || {TgId, MucJid} <- Rooms],
%%	self() ! sub_linked_rooms, %% subscribe to all linked rooms %% TODO to think about subscribe and structure of #muc_state.state
	self() ! enter_linked_rooms, %% enter to all linked rooms
	{ok, #tg_state{bot_id = BotId, bot_name = BotName, component = Component, nick = Nick, token = Token, rooms = NewRooms}}.

%% Function that handles information message received from the group chat of Telegram
handle_info({pe4kin_update, BotName,
	#{<<"message">> :=
		#{<<"chat">> := #{<<"type">> := <<"group">>, <<"id">> := CurChatId},
		  <<"from">> := #{<<"username">> := TgUserName},
			<<"message_id">> := Id,
		  <<"text">> := Text}}} = TgMsg, Client,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms, component = Component} = State) ->
	ct:print("tg msg groupchat: ~p", [TgMsg]),
		[begin
			 OriginId = ebridgebot:gen_uuid(),
			 escalus:send(Client, xmpp:encode(#message{id = OriginId, type = groupchat, from = jid:decode(Component), to = jid:decode(MucJid),
				 body = [#text{data = <<TgUserName/binary, ":\n", Text/binary>>}], sub_els = [#origin_id{id = OriginId}]})),
			 write_link(BotId, OriginId, ChatId, Id)
		 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = S} <- Rooms, CurChatId == ChatId andalso (S == in orelse S == subscribed)],
	{ok, State};
handle_info({pe4kin_update, BotName,
	#{<<"edited_message">> :=
	#{<<"chat">> := #{<<"type">> := <<"group">>, <<"id">> := CurChatId},
		<<"from">> := #{<<"username">> := TgUserName},
		<<"message_id">> := Id,
		<<"text">> := Text}}} = TgMsg, Client,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms, component = Component} = State) ->
	ct:print("edit tg msg groupchat: ~p", [TgMsg]),
	[case get_replace_id(#tg_id{chat_id = ChatId, id = Id}, BotId) of
		 [] -> ok;
		 ReplaceId ->
			 Pkt = edit_msg(jid:decode(Component), jid:decode(MucJid), <<TgUserName/binary, ":\n", Text/binary>>, ReplaceId),
			 escalus:send(Client, xmpp:encode(Pkt))
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = S} <- Rooms, CurChatId == ChatId andalso (S == in orelse S == subscribed)],
	{ok, State};
handle_info({pe4kin_update, BotName, #{<<"message">> := _} = TgMsg}, _Client, #tg_state{bot_name = BotName} = State) ->
	ct:print("tg msg: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_update, BotName, TgMsg}, _Client, #tg_state{bot_name = BotName} = State) ->
	ct:print("tg msg2: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_send, ChatId, Text}, _Client, #tg_state{bot_name = BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	ct:print("!!pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info({link_rooms, TgRoomId, MucJid}, _Client, #tg_state{rooms = Rooms} = State) ->
	LMucJid = string:lowercase(MucJid),
	NewRooms =
		case [exist || #muc_state{group_id = GId, muc_jid = J} <- Rooms, TgRoomId == GId, LMucJid == J] of
			[] -> [#muc_state{group_id = TgRoomId, muc_jid = LMucJid} | Rooms];
			_ -> Rooms
		end,
	{ok, State#tg_state{rooms = NewRooms}};
handle_info({enter_groupchat, MucJid}, Client, #tg_state{component = Component, nick = Nick} = State) when is_binary(MucJid) ->
	EnterPresence = #presence{from = jid:make(Component), to = jid:replace_resource(jid:decode(MucJid), Nick), sub_els = [#muc{}]},
	escalus:send(Client, xmpp:encode(EnterPresence)),
	{ok, State};
handle_info({subscribe_component, MucJid}, Client, #tg_state{component = Component, nick = Nick} = State) when is_binary(MucJid) ->
	ct:print("subscribe_component: ~p", [MucJid]),
	escalus:send(Client, xmpp:encode(sub_iq(jid:make(Component), jid:decode(MucJid), Nick))),
	{ok, State};
handle_info(enter_linked_rooms, Client, #tg_state{rooms = Rooms} = State) ->
	NewRooms =
		lists:foldr(
			fun(#muc_state{muc_jid = MucJid, state = out} = MucState, Acc) ->
					case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
						#muc_state{} -> ok;
						false -> handle_info({enter_groupchat, MucJid}, Client, State)
					end,
					[MucState#muc_state{state = pending} | Acc];
				(MucState, Acc) ->
					[MucState | Acc]
			end, [], Rooms),
	{ok, State#tg_state{rooms = NewRooms}};
handle_info(sub_linked_rooms, Client, #tg_state{rooms = Rooms} = State) ->
	NewRooms =
		lists:foldr(
			fun(#muc_state{muc_jid = MucJid, state = out} = MucState, Acc) ->
					case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
						#muc_state{} -> ok;
						false -> handle_info({subscribe_component, MucJid}, Client, State)
					end,
					[MucState#muc_state{state = subscribed} | Acc];
				(MucState, Acc) ->
					[MucState | Acc]
			end, [], Rooms),
	{ok, State#tg_state{rooms = NewRooms}};
handle_info({state, Pid}, _Client, State) ->
	Pid ! {state, State},
	{ok, State};
handle_info(Info, _Client, State) ->
	ct:print("handle component: ~p", [Info]),
	{ok, State}.

process_stanza(#xmlel{} = Stanza, Client, State) ->
	process_stanza(xmpp:decode(Stanza), Client, State);
process_stanza(#presence{type = available, from = #jid{} = CurMucJID, to = To} = Pkt,
	_Client, #tg_state{rooms = Rooms, component = ComponentJid} = State) ->
	case {jid:encode(To), xmpp:get_subtag(Pkt, #muc_user{})} of
		{ComponentJid, #muc_user{items = [#muc_item{jid = To}]}} ->
			CurMucJid = jid:encode(jid:remove_resource(CurMucJID)),
			NewRooms =
				lists:foldr(
					fun(#muc_state{muc_jid = MucJid} = MucState, Acc) when MucJid == CurMucJid ->
							[MucState#muc_state{state = in} | Acc]; %% set muc_state as in for this presence
					   (MucState, Acc) ->
						    [MucState | Acc]
					end, [], Rooms),
			{ok, State#tg_state{rooms = NewRooms}};
		_ -> {ok, State}
	end;
process_stanza(#message{type = groupchat, from = #jid{resource = Nick} = From, body = [#text{data = Text}]} = Pkt, _Client,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms, nick = ComponentNick} = State) when Nick /= ComponentNick ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	ct:print("msg to tg: ~p", [Pkt]),
	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	[try
		 case xmpp:get_subtag(Pkt, #replace{}) of
			 #replace{id = ReplaceId} ->
				 ct:print("edit msg to tg"),
				 case get_replace_id(#xmpp_id{id = ReplaceId, bot_id = BotId}, ChatId) of
					 [] -> ok;
					 Id ->
						 pe4kin:edit_message(BotName, #{chat_id => ChatId, message_id => Id, text => <<Nick/binary, ":\n", Text/binary>>})
				 end;
			 _ ->
				 {ok, #{<<"message_id">> := Id}} =
					 pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<Nick/binary, ":\n", Text/binary>>}),
				 write_link(BotId, OriginId, ChatId, Id)
		 end
	 catch
		 E : R ->
			 ct:print("ERROR: ~p: ~p", [E, R])
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(Stanza, _Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	ct:print("handle component stanza: ~p", [Stanza]),
	{ok, State}.

terminate(Reason, State) ->
	ct:print("terminate/2 ~p", [{Reason, State}]),
	ok.

-spec stop(atom()) -> 'ok'.
stop(BotId) ->
	Pid = pid(BotId),
	escalus_component:stop(Pid, <<"stopped">>).

state(Pid) when is_pid(Pid) ->
	Pid ! {state, self()},
	receive {state, State} -> State after 1000 -> {error, timeout} end;
state(BotId) ->
	state(pid(BotId)).

pid(BotId) ->
	Children = supervisor:which_children(ebridgebot_sup),
	case lists:keyfind(BotId, 1, Children) of
		{_, Pid, _, _} -> Pid;
		_ -> {error, bot_not_found}
	end.

enter_groupchat(BotId, MucJid) ->
	pid(BotId) ! {enter_groupchat, MucJid}.

enter_linked_rooms(BotId) ->
	pid(BotId) ! enter_linked_rooms.

send(BotId, ChatId, Text) ->
	pid(BotId) ! {pe4kin_send, ChatId, Text}.

sub_iq(From, To, Nick) ->
	sub_iq(From, To, Nick, <<>>).
sub_iq(From, To, Nick, Password) ->
	#iq{type = set, from = From, to = To,
		sub_els = [#muc_subscribe{nick = Nick, password = Password,
			events = [?NS_MUCSUB_NODES_MESSAGES,
				?NS_MUCSUB_NODES_AFFILIATIONS,
				?NS_MUCSUB_NODES_SUBJECT,
				?NS_MUCSUB_NODES_CONFIG]}]}.

edit_msg(From, To, Text, ReplaceId) ->
	OriginId = ebridgebot:gen_uuid(),
	#message{id = OriginId, type = groupchat, from = From, to = To, body = [#text{data = Text}],
		sub_els = [#origin_id{id = OriginId}, #replace{id = ReplaceId}]}.

subscribe_component(BotId, MucJid) ->
	pid(BotId) ! {subscribe_component, MucJid}.

write_link(BotId, OriginId, ChatId, Id) ->
	mnesia:dirty_write(
		#xmpp_link{xmpp_id = #xmpp_id{bot_id = BotId, id = OriginId},
			uid = #tg_id{id = Id, chat_id = ChatId}}).

get_replace_id(#tg_id{} = TgId, BotId) ->
	case mnesia:dirty_index_read(xmpp_link, TgId, #xmpp_link.uid) of
		[#xmpp_link{xmpp_id = #xmpp_id{id = OriginId, bot_id = BotId}}] ->
			OriginId;
		_ -> []
	end;
get_replace_id(#xmpp_id{} = XmppId, ChatId) ->
	case mnesia:dirty_read(xmpp_link, XmppId) of
		[#xmpp_link{uid = #tg_id{chat_id = ChatId, id = Id}}] ->
			Id;
		_ -> []
	end.