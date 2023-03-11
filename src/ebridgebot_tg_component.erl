-module(ebridgebot_tg_component).
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-export([init/1, handle_info/3, process_stanza/3, terminate/2]).

bot_table(BotId) -> %% generate table name for bot
	list_to_atom(atom_to_list(BotId)++"_link").

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotId, BotName, BotToken, Component, Nick, Rooms] =
		[proplists:get_value(K, Args) ||
			K <- [bot_id, name, token, component, nick, linked_rooms]],
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
	NewRooms = [#muc_state{group_id = TgId, muc_jid = MucJid} || {TgId, MucJid} <- Rooms],
	self() ! enter_linked_rooms, %% enter to all linked rooms

	application:start(mnesia),
	mnesia:create_table(bot_table(BotId),
		[{attributes, record_info(fields, xmpp_link)},
			{index, [xmpp_id, uid]},
			{disc_copies, [node()]}]),

	{ok, #tg_state{bot_id = BotId, bot_name = BotName, component = Component, nick = Nick, token = BotToken, rooms = NewRooms}}.

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
		 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
			CurChatId == ChatId andalso (E == in orelse S == subscribed)], %% TODO maybe remove 'subscribed' state
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
			 Pkt = #message{id = OriginId} = edit_msg(jid:decode(Component), jid:decode(MucJid), <<TgUserName/binary, ":\n", Text/binary>>, ReplaceId),
			 escalus:send(Client, xmpp:encode(Pkt)),
			 write_link(BotId, OriginId, ChatId, Id) %% TODO maybe you don't need to write because there is no retract from Telegram
	 end || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms, CurChatId == ChatId andalso (E == in orelse S == subscribed)],
	{ok, State};
handle_info({pe4kin_update, BotName, #{<<"message">> := _} = TgMsg}, _Client, #tg_state{bot_name = BotName} = State) ->
	ct:print("tg msg: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_update, BotName, TgMsg}, _Client, #tg_state{bot_name = BotName} = State) ->
	ct:print("tg msg2: ~p", [TgMsg]),
	{ok, State};
handle_info({pe4kin_send, ChatId, Text}, _Client, #tg_state{bot_name = BotName} = State) ->
	Res = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Text}),
	ct:print("pe4kin_send: ~p", [Res]),
	{ok, State};
handle_info({link_rooms, TgRoomId, MucJid}, _Client, #tg_state{rooms = Rooms} = State) ->
	LMucJid = string:lowercase(MucJid),
	NewRooms =
		case [ok || #muc_state{group_id = GId, muc_jid = J} <- Rooms, TgRoomId == GId, LMucJid == J] of
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
			fun(#muc_state{muc_jid = MucJid, state = {out, S}} = MucState, Acc) ->
					case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
						#muc_state{} -> ok;
						false -> handle_info({enter_groupchat, MucJid}, Client, State)
					end,
					[MucState#muc_state{state = {pending, S}} | Acc];
				(MucState, Acc) ->
					[MucState | Acc]
			end, [], Rooms),
	{ok, State#tg_state{rooms = NewRooms}};
handle_info(sub_linked_rooms, Client, #tg_state{rooms = Rooms} = State) ->
	NewRooms =
		lists:foldr(
			fun(#muc_state{muc_jid = MucJid, state = {E, unsubscribed}} = MucState, Acc) ->
					case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
						#muc_state{} -> ok;
						false -> handle_info({subscribe_component, MucJid}, Client, State)
					end,
					[MucState#muc_state{state = {E, subscribed}} | Acc];
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
	_Client, #tg_state{bot_id = BotId, rooms = Rooms, component = ComponentJid} = State) ->
	?dbg("presence available: ~p", [Pkt]),
	case {jid:encode(To), xmpp:get_subtag(Pkt, #muc_user{})} of
		{ComponentJid, #muc_user{items = [#muc_item{jid = To}]}} ->
			CurMucJid = jid:encode(jid:remove_resource(CurMucJID)),
			case lists:keyfind(CurMucJid, #muc_state.muc_jid, Rooms) of
				#muc_state{state = {_, S}} = MucState ->
					NewRooms = lists:keyreplace(CurMucJid, #muc_state.muc_jid, Rooms, MucState#muc_state{state = {in, S}}),
					?dbg("msg state in ~p = ~p", [BotId, State#tg_state{rooms = NewRooms}]),
					{ok, State#tg_state{rooms = NewRooms}};
				_ ->
					?dbg("user not found in ~p: ~p in ~p", [BotId, CurMucJid, State]),
					{ok, State}
			end;
		_ ->
			{ok, State}
	end;
process_stanza(#message{type = groupchat, from = #jid{resource = Nick}} = Pkt, _Client,
				#tg_state{nick = ComponentNick} = State) when Nick /= ComponentNick ->
	(ebridgebot:tag_decorator([#replace{}, #apply_to{}, #origin_id{}], [Pkt, State], ?MODULE, process_stanza))();
process_stanza(Stanza, _Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	ct:print("handle component stanza: ~p", [Stanza]),
	{ok, State}.

%% callbacks for ebridgebot:tag_decorator
process_stanza(#origin_id{id = OriginId}, [#message{type = groupchat} = Pkt, #tg_state{bot_id = BotId} = State]) ->
	case dirty_index_read(BotId, OriginId, #xmpp_link.xmpp_id) of
		[_ | _] -> {ok, State}; %% not send to tg if messages already linked
		[] -> process_stanza([Pkt, State])
	end;
process_stanza(#replace{id = ReplaceId}, [#message{type = groupchat, from = #jid{resource = Nick} = From, body = [#text{data = Text}]} = Pkt,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms} = State]) ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	ct:print("replace msg to tg: ~p", [Pkt]),
	[case get_replace_id(ReplaceId, {ChatId, BotId}) of
		 [] -> ok;
		 Id ->
			 #origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
			 pe4kin:edit_message(BotName, #{chat_id => ChatId, message_id => Id, text => <<Nick/binary, ":\n", Text/binary>>}),
			 write_link(BotId, OriginId, ChatId, Id)
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(#apply_to{id = RetractId, sub_els = [#retract{}]}, [#message{type = groupchat, from = From} = Pkt,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms} = State]) -> %% retract message from tg chat
	MucFrom = jid:encode(jid:remove_resource(From)),
	ct:print("retract msg to tg: ~p", [Pkt]),
	[case get_replace_id(RetractId, {ChatId, BotId}) of
		 [] -> ok;
		 Id ->
			 pe4kin:delete_message(BotName, #{chat_id => ChatId, message_id => Id}),
			 Table = bot_table(BotId),
			 Links = dirty_index_read(BotId, #tg_id{id = Id, chat_id = ChatId}, #xmpp_link.uid),
			 [mnesia:dirty_delete(Table, Time) || #xmpp_link{time = Time} <- Links]
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(_, [#message{} = Pkt, #tg_state{} = State]) ->
	ct:print("unexpected msg to tg: ~p", [Pkt]),
	{ok, State}.
process_stanza([#message{type = groupchat, from = #jid{resource = Nick} = From, body = [#text{data = Text}]} = Pkt,
	#tg_state{bot_id = BotId, bot_name = BotName, rooms = Rooms} = State]) ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	ct:print("group msg to tg: ~p", [Pkt]),
	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	[case pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<Nick/binary, ":\n", Text/binary>>}) of
		 {ok, #{<<"message_id">> := Id}} ->
		    write_link(BotId, OriginId, ChatId, Id);
		 Err ->
			 ct:print("ERROR: ~p", [Err])
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza([#message{} = Pkt, #tg_state{} = State]) ->
	ct:print("group msg to tg: 2: ~p\n~p", [Pkt, State]),
	{ok, State}.

terminate(Reason, State) ->
	ct:print("terminate/2 ~p", [{Reason, State}]),
	ok.

%% component API
-spec stop(atom()) -> 'ok'.
stop(BotId) ->
	Pid = pid(BotId),
	escalus_component:stop(Pid, <<"stopped">>).

state(BotId) ->
	pid(BotId) ! {state, self()},
	receive {state, State} -> State after 1000 -> {error, timeout} end.

pid(Pid) when is_pid(Pid) ->
	Pid;
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
		setelement(1, #xmpp_link{xmpp_id = OriginId, uid = #tg_id{id = Id, chat_id = ChatId}}, bot_table(BotId))).

get_replace_id(#tg_id{} = TgId, BotId) ->
	case dirty_index_read(BotId, TgId, #xmpp_link.uid) of
		[#xmpp_link{xmpp_id = OriginId} | _] ->
			OriginId;
		_ -> []
	end;
get_replace_id(OriginId, {ChatId, BotId}) when is_binary(OriginId) ->
	case dirty_index_read(BotId, OriginId, #xmpp_link.xmpp_id) of
		[#xmpp_link{uid = #tg_id{chat_id = ChatId, id = Id}} | _] ->
			Id;
		_ -> []
	end.

dirty_index_read(BotId, Key, Field) ->
	[setelement(1, R, xmpp_link) || R <- mnesia:dirty_index_read(bot_table(BotId), Key, Field)].