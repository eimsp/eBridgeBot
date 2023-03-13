-module(ebridgebot_component).

%% API
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").

-export([init/1, handle_info/3, process_stanza/3, terminate/2]).

init(Args) ->
	[BotId, BotName, Component, Nick, Rooms, Module] =
		[proplists:get_value(K, Args) ||
			K <- [bot_id, name, component, nick, linked_rooms, module]],
	NewRooms = [#muc_state{group_id = TgId, muc_jid = MucJid} || {TgId, MucJid} <- Rooms],
	self() ! enter_linked_rooms, %% enter to all linked rooms

	application:start(mnesia),
	mnesia:create_table(ebridgebot:bot_table(BotId),
		[{attributes, record_info(fields, xmpp_link)},
			{index, [xmpp_id, uid]},
			{disc_copies, [node()]}]),
	{ok, State} = Module:init(Args),
	{ok, State#{bot_id => BotId, bot_name => BotName, component => Component, nick => Nick, rooms => NewRooms, module => Module}}.

handle_info({link_rooms, ChatId, MucJid}, _Client, #{rooms := Rooms} = State) ->
	LMucJid = string:lowercase(MucJid),
	NewRooms =
		case [ok || #muc_state{group_id = ChatId2, muc_jid = J} <- Rooms, ChatId == ChatId2, LMucJid == J] of
			[] -> [#muc_state{group_id = ChatId, muc_jid = LMucJid} | Rooms];
			_ -> Rooms
		end,
	{ok, State#{rooms => NewRooms}};
handle_info({enter_groupchat, MucJid}, Client, #{component := Component, nick := Nick} = State) when is_binary(MucJid) ->
	EnterPresence = #presence{from = jid:make(Component), to = jid:replace_resource(jid:decode(MucJid), Nick), sub_els = [#muc{}]},
	escalus:send(Client, xmpp:encode(EnterPresence)),
	{ok, State};
handle_info({subscribe_component, MucJid}, Client, #{component := Component, nick := Nick} = State) when is_binary(MucJid) ->
	?dbg("subscribe_component: ~p", [MucJid]),
	escalus:send(Client, xmpp:encode(sub_iq(jid:make(Component), jid:decode(MucJid), Nick))),
	{ok, State};
handle_info(enter_linked_rooms, Client, #{rooms := Rooms} = State) ->
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
	{ok, State#{rooms => NewRooms}};
handle_info(sub_linked_rooms, Client, #{rooms := Rooms} = State) ->
	NewRooms =
		lists:foldr( %% TODO combine enter_linked_rooms and sub_linked_rooms
			fun(#muc_state{muc_jid = MucJid, state = {E, unsubscribed}} = MucState, Acc) ->
				case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
					#muc_state{} -> ok;
					false -> handle_info({subscribe_component, MucJid}, Client, State)
				end,
				[MucState#muc_state{state = {E, subscribed}} | Acc];
				(MucState, Acc) ->
					[MucState | Acc]
			end, [], Rooms),
	{ok, State#{rooms => NewRooms}};
handle_info({state, Pid}, _Client, State) ->
	Pid ! {state, State},
	{ok, State};
handle_info(Info, Client, #{module := Module} = State) ->
	Module:handle_info(Info, Client, State);
handle_info(Info, _Client, State) ->
	?dbg("handle component: ~p", [Info]),
	{ok, State}.

process_stanza(#xmlel{} = Stanza, Client, State) ->
	process_stanza(xmpp:decode(Stanza), Client, State);
process_stanza(#presence{type = Type, from = #jid{} = CurMucJID, to = #jid{server = ComponentJid} = To} = Pkt,
	_Client, #{bot_id := BotId, rooms := Rooms, component := ComponentJid} = State) when Type == available; Type == unavailable ->
	?dbg("presence: ~p", [Pkt]),
	Enter = case Type of available -> in; unavailable -> out end,
	case {jid:encode(To), xmpp:get_subtag(Pkt, #muc_user{})} of
		{ComponentJid, #muc_user{items = [#muc_item{jid = To}]}} ->
			CurMucJid = jid:encode(jid:remove_resource(CurMucJID)),
			case lists:keyfind(CurMucJid, #muc_state.muc_jid, Rooms) of
				#muc_state{state = {_, S}} = MucState ->
					NewRooms = lists:keyreplace(CurMucJid, #muc_state.muc_jid, Rooms, MucState#muc_state{state = {Enter, S}}),
					{ok, State#{rooms => NewRooms}};
				_ ->
					?dbg("user not found in ~p: ~p in ~p", [BotId, CurMucJid, State]),
					{ok, State}
			end;
		_ ->
			{ok, State}
	end;
process_stanza(#message{} = Pkt, Client, #{} = State) ->
	(ebridgebot:tag_decorator([#ps_event{}, #replace{}, #apply_to{}, #origin_id{}], [Pkt, State, Client], ?MODULE, process_stanza))();
process_stanza(Stanza, _Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	?dbg("handle component stanza: ~p", [Stanza]),
	{ok, State}.

%% callbacks for ebridgebot:tag_decorator
process_stanza(#ps_event{items = #ps_items{node = ?NS_MUCSUB_NODES_MESSAGES, items = [#ps_item{sub_els = [#xmlel{name = <<"message">>} = Pkt]}]}},
	[#message{type = normal, from = #jid{resource = <<>>}, to = #jid{resource = <<>>}}, #{} = State, Client]) -> %% event message if subscribed
	?dbg("handle event sub message: ~p", [Pkt]),
	process_stanza(Pkt,  Client, State);
process_stanza(#ps_event{} = Event,	[#message{} = _Pkt, #{} = State | _]) ->
	?dbg("handle event message: ~p", [Event]),
	%% TODO not implemented
	{ok, State};
process_stanza(_, [#message{type = groupchat, from = #jid{resource = ComponentNick}} = Pkt,
	#{nick := ComponentNick} = State | _]) -> %% ignore echo messages from xmpp client
	?dbg("handle message: ~p", [Pkt]),
	{ok, State};
process_stanza(#origin_id{id = OriginId}, [#message{type = groupchat} = Pkt, #{bot_id := BotId} = State | _]) ->
	case index_read(BotId, OriginId, #xmpp_link.xmpp_id) of
		[_ | _] -> {ok, State}; %% not send to third party client if messages already linked
		[] -> process_stanza([Pkt, State])
	end;
process_stanza(#replace{}, [{uid, Uid}, #message{type = groupchat, from = #jid{resource = Nick}, body = [#text{data = Text}]} = Pkt,
		#{bot_id := BotId, module := Module} = State | _]) -> %% edit message from xmpp groupchat with uid
	?dbg("replace: ~p", [Pkt]),
	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	Module:edit_message(State#{uid => Uid}, <<Nick/binary, ":\n", Text/binary>>),
	write_link(BotId, OriginId, Uid),
	{ok, State};
process_stanza(#apply_to{sub_els = [#retract{}]}, [{uid, Uid}, #message{type = groupchat} = Pkt,
		#{bot_id := BotId, module := Module} = State | _]) -> %% retract message from xmpp groupchat
	?dbg("retract: ~p", [Pkt]),
	Module:delete_message(State#{uid => Uid}),
	Table = ebridgebot:bot_table(BotId),
	[mnesia:dirty_delete(Table, TimeId) || #xmpp_link{time = TimeId} <- index_read(BotId, Uid, #xmpp_link.uid)],
	{ok, State};
process_stanza(Tag, [#message{type = groupchat, from = #jid{} = From} = Pkt, #{bot_id := BotId, rooms := Rooms, module := Module} = State | T] = S)
	when is_record(Tag, replace); is_record(Tag, apply_to) -> %% edit message from xmpp groupchat
	MucFrom = jid:encode(jid:remove_resource(From)),
	?dbg("replace or retract msg to third party client: ~p", [Pkt]),
	OriginId = element(#apply_to.id = #replace.id, Tag), %% #apply_to.id == #replace.id
	Links = index_read(BotId, OriginId, #xmpp_link.xmpp_id),
	[case lists:filter(Module:link_pred(State#{group_id => ChatId}), Links) of
		 [#xmpp_link{uid = Uid} | _] ->
			 process_stanza(Tag, [{uid, Uid} | S]);
		 _ -> ok
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(_, [#message{} = Pkt, #{} = State | _]) ->
	?dbg("unexpected msg from xmpp server: ~p", [Pkt]),
	{ok, State}.
process_stanza([#message{type = groupchat, from = #jid{resource = Nick} = From, body = [#text{data = Text}]} = Pkt,
	#{bot_id := BotId, rooms := Rooms, module := Module} = State | _]) ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	?dbg("send to third party client: ~p", [Pkt]),
	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	[case Module:send_message(State#{chat_id => ChatId}, <<Nick/binary, ":\n", Text/binary>>) of
		 {ok, Uid} -> write_link(BotId, OriginId, Uid);
		 Err -> Err
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza([#message{} = Pkt, #{} = State | _]) ->
	?dbg("unhandled message: ~p\n~p", [Pkt, State]),
	{ok, State}.

terminate(Reason, State) ->
	?dbg("terminate/2:\n~p", [{Reason, State}]),
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

write_link(BotId, OriginId, Uid) ->
	mnesia:dirty_write(
		setelement(1, #xmpp_link{xmpp_id = OriginId, uid = Uid}, ebridgebot:bot_table(BotId))).

index_read(BotId, Key, Field) ->
	[setelement(1, R, xmpp_link) || R <- mnesia:dirty_index_read(ebridgebot:bot_table(BotId), Key, Field)].