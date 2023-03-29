-module(ebridgebot_component).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").

%% API
-export([start/2, start_link/1, start_link/2, init/1, handle_info/3, process_stanza/3, process_stanza/2, process_stanza/1, terminate/2, stop/1,
	state/1, state/2, pid/1, filter_pred/1]).

start(BotId, Args) ->
	escalus_component:start({local, BotId}, ebridgebot_component, Args, [{bot_id, BotId} | Args]).

start_link(BotId, Args) ->
	start_link([{bot_id, BotId} | Args]).
start_link(Args) ->
	escalus_component:start_link(?MODULE, Args, Args).

init(Args) ->
	[BotId, BotName, Component, Nick, Rooms, Module] =
		[proplists:get_value(K, Args) ||
			K <- [bot_id, name, component, nick, linked_rooms, module]],
	UploadHost = proplists:get_value(upload_host, Args, <<"upload.localhost">>),
	UploadEndpoint = proplists:get_value(upload_endpoint, Args),
	LinkedRooms = [#muc_state{group_id = TgId, muc_jid = MucJid} || {TgId, MucJid} <- Rooms],

	application:start(mnesia),
	mnesia:create_table(ebridgebot:bot_table(BotId),
		[{attributes, record_info(fields, xmpp_link)},
			{index, [origin_id, mam_id, uid]},
			{disc_copies, [node()]}]),

	self() ! {linked_rooms, presence, available}, %% enter to all linked rooms

	{ok, State} = Module:init(Args),
	{ok, State#{bot_id => BotId, bot_name => BotName, component => Component, nick => Nick,
		rooms => LinkedRooms, module => Module, upload_host => UploadHost, upload_endpoint => UploadEndpoint, upload => #{}}}.

handle_info({link_scheduler, ClearInterval, LifeSpan} = Info, _Client, State) -> %% ClearInterval and LifeSpan in milliseconds
	?dbg("link_scheduler", []),
	catch erlang:cancel_timer(maps:get(link_scheduler_ref, State)),
	handle_info({remove_old_links, erlang:system_time(microsecond) - LifeSpan * 1000}, _Client, State),
	{ok, TRef} = timer:send_after(ClearInterval, Info),
	{ok, State#{link_scheduler_ref => TRef}};
handle_info(stop_link_scheduler, _Client, #{link_scheduler_ref := TRef} = State) ->
	?dbg("stop_link_scheduler", []),
	catch erlang:cancel_timer(TRef),
	{ok, maps:remove(link_scheduler_ref, State)};
handle_info({remove_old_links, OldestTS}, _Client, #{bot_id := BotId} = State) ->
	?dbg("remove_old_links", []),
	MatchSpec = [{{Table = ebridgebot:bot_table(BotId), '$1', '_', '_', '_'}, [{'<', '$1', OldestTS}], ['$1']}],
	spawn(fun() -> [mnesia:dirty_delete(Table, K) || K <- mnesia:dirty_select(Table, MatchSpec)] end),
	{ok, State};

handle_info({link_rooms, ChatId, MucJid}, _Client, #{rooms := Rooms} = State) ->
	LMucJid = string:lowercase(MucJid),
	NewRooms =
		case [ok || #muc_state{group_id = ChatId2, muc_jid = J} <- Rooms, ChatId == ChatId2, LMucJid == J] of
			[] -> [#muc_state{group_id = ChatId, muc_jid = LMucJid} | Rooms];
			_ -> Rooms
		end,
	{ok, State#{rooms => NewRooms}};
handle_info({presence, Type, MucJid} = Info, Client, #{component := Component, nick := Nick} = State)
	when Type == available; Type == unavailable ->
	?dbg("handle: ~p", [Info]),
	EnterPresence = #presence{type = Type, from = jid:make(Component), to = jid:replace_resource(jid:decode(MucJid), Nick), sub_els = [#muc{}]},
	escalus:send(Client, xmpp:encode(EnterPresence)),
	{ok, State};
handle_info({event, SubAction, MucJid} = Info, Client, #{component := Component, nick := Nick} = State)
	when SubAction == subscribe; SubAction == unsubscribe ->
	?dbg("handle: ~p", [Info]),
	escalus:send(Client, xmpp:encode(ebridgebot:iq(SubAction, jid:make(Component), jid:decode(MucJid), Nick))),
	{ok, State};
handle_info({linked_rooms, Type, Action} = Info, Client, #{rooms := Rooms} = State) when Type == presence; Type == event ->
	?dbg("handle: ~p", [Info]),
%%	escalus_connection:set_filter_predicate(Client, filter_pred(State)),
	{I, Prev, Next} =
		case {Type, Action} of
			{presence, available} -> {1, out, pending};
			{presence, unavailable} -> {1, in, pending};
			{event, subscribe} -> {2, unsubscribed, subscribed};
			{event, unsubscribe} -> {2, subscribed, unsubscribed}
		end,
	LinkedRooms =
		lists:foldr( %% sync states of rooms
			fun(#muc_state{muc_jid = MucJid, state = S} = MucState, Acc) when element(I, S) == Prev ->
				case lists:keyfind(MucJid, #muc_state.muc_jid, Acc) of
					#muc_state{} -> ok;
					false -> handle_info({Type, Action, MucJid}, Client, State)
				end,
				[MucState#muc_state{state = setelement(I, S, Next)} | Acc];
				(MucState, Acc) ->
					[MucState | Acc]
			end, [], Rooms),
	{ok, State#{rooms => LinkedRooms}};
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
process_stanza(#iq{id = FileId, type = result, from = #jid{server = UploadHost}, to = #jid{server = ComponentJid},
					sub_els = [#upload_slot_0{get = GetURL, put = PutURL, xmlns = ?NS_HTTP_UPLOAD_0}]} = IQ,
		_Client,
		#{bot_id := BotId, module := Module, component := ComponentJid, upload_host := UploadHost, upload := Upload} = State)
	when is_map_key(FileId, Upload) ->
	#{FileId := #upload_info{nick = Nick, content_type = ContentType, file_path = FilePath,
		muc_jids = RoomJids, caption = Caption, uid = Uid, send_type = SendType}} = Upload,
	?dbg("upload slot: ~p", [IQ]),
	Pid = self(),
	spawn( %% async get and put data
		fun() ->
			try %% TODO error handling
				{ok, Data} = Module:get_file(State#{file_path => FilePath}), %% TODO set get_file more universal
				{ok, {{"HTTP/1.1", 201, _}, _, _}} =
					httpc:request(put, {binary_to_list(PutURL), [], binary_to_list(ContentType), Data}, [], []),
				[ebridgebot:send(SendType, Pid, BotId, ComponentJid, MucJid, Uid, Nick, <<Caption/binary, GetURL/binary>>) || MucJid <- RoomJids]
			catch
				E : R ->
					?err("ERROR:~p: ~p", [E, R])
			end
		end),
	{ok, State#{upload => maps:remove(FileId, Upload)}};
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
process_stanza(#origin_id{id = OriginId}, [#message{type = groupchat} = Pkt, #{bot_id := BotId} = State | _]) ->
	case ebridgebot:upd_links(BotId, OriginId, xmpp:get_subtag(Pkt, #mam_archived{})) of
		[_ | _] -> {ok, State}; %% not send to third party client if messages already linked
		[] -> process_stanza([Pkt, State])
	end;
process_stanza(#replace{}, [{uid, Uid}, #message{type = groupchat, from = #jid{resource = Nick}, body = [#text{data = Text}]} = Pkt,
	#{bot_id := BotId, module := Module} = State | _]) -> %% edit message from xmpp groupchat with uid
	?dbg("replace: ~p", [Pkt]),
	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	Module:edit_message(State#{uid => Uid, text => <<Nick/binary, ":\n", Text/binary>>}),
	ebridgebot:write_link(BotId, OriginId, Uid, xmpp:get_subtag(Pkt, #mam_archived{})),
	{ok, State};
process_stanza(#apply_to{sub_els = [#moderated{sub_els = [#retract{} | _]}]}, [{uid, _Uid}, Pkt | _] = Args) -> %% retract message from groupchat by moderator
	?dbg("moderator retract: ~p", [Pkt]),
	process_stanza(#apply_to{sub_els = [#retract{}]}, Args);
process_stanza(#apply_to{sub_els = [#retract{}]}, [{uid, Uid}, #message{type = groupchat} = Pkt,
	#{bot_id := BotId, module := Module} = State | _]) -> %% retract message from xmpp groupchat
	?dbg("retract: ~p", [Pkt]),
	Module:delete_message(State#{uid => Uid}),
	Table = ebridgebot:bot_table(BotId),
	[mnesia:dirty_delete(Table, TimeId) || #xmpp_link{time = TimeId} <- ebridgebot:index_read(BotId, Uid, #xmpp_link.uid)],
	{ok, State};
process_stanza(Tag, [#message{type = groupchat, from = #jid{} = From} = Pkt, #{bot_id := BotId, rooms := Rooms, module := Module} = State | _] = Args)
	when is_record(Tag, replace); is_record(Tag, apply_to) -> %% edited or moderated message from xmpp groupchat
	?dbg("replace or retract msg to third party client: ~p", [Pkt]),
	MucFrom = jid:encode(jid:remove_resource(From)),
	Id = element(#apply_to.id = #replace.id, Tag), %% #apply_to.id == #replace.id
	Attr = case Tag of #apply_to{sub_els = [#moderated{}]} -> #xmpp_link.mam_id; _ -> #xmpp_link.origin_id end,
	Links = ebridgebot:index_read(BotId, Id, Attr),
	[case lists:filter(Module:link_pred(State#{group_id => ChatId}), Links) of
		 [#xmpp_link{uid = Uid} | _] ->
			 process_stanza(Tag, [{uid, Uid} | Args]);
		 [] when is_record(Tag, replace) ->
			 process_stanza(xmpp:get_subtag(Pkt, #origin_id{}), Args); %% send new message if the replaced message does not exist
		 [] -> ok
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(_, [#message{} = Pkt, #{} = State | _]) ->
	?dbg("unexpected msg from xmpp server: ~p", [Pkt]),
	{ok, State}.
process_stanza([#message{type = groupchat, from = #jid{resource = Nick} = From, body = [#text{data = Text}]} = Pkt,
	#{bot_id := BotId, rooms := Rooms, module := Module, upload_endpoint := UploadEndpoint} = State | _]) ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	?dbg("send to third party client: ~p", [Pkt]),
	{Fun, TmpState} =
		case catch binary:match(Text, [UploadEndpoint]) of
			Found when is_binary(UploadEndpoint), Found /= nomatch -> %% TODO if endpoint path has port then tg does not allow upload
				{send_data, State#{mime => hd(mimetypes:filename(Text)), file_uri => Text, caption => <<Nick/binary, ":">>}};
			_ ->
				{send_message, State#{text => <<Nick/binary, ":\n", Text/binary>>}}
		end,

	#origin_id{id = OriginId} = xmpp:get_subtag(Pkt, #origin_id{}),
	[case Module:Fun(TmpState#{chat_id => ChatId}) of
		 {ok, Uid} ->
			 ebridgebot:write_link(BotId, OriginId, Uid, xmpp:get_subtag(Pkt, #mam_archived{}));
		 Err -> Err
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza([#message{} = Pkt, #{} = State | _]) ->
	?dbg("unhandled message: ~p\n~p", [Pkt, State]),
	{ok, State}.

terminate(Reason, State) ->
	?err("terminate/2:\n~p", [{Reason, State}]),
	ok.

%% component API
-spec stop(atom()) -> 'ok'.
stop(BotId) ->
	Pid = pid(BotId),
	escalus_component:stop(Pid, <<"stopped">>).

-spec state(atom()) -> #{} | {error, timeout}.
state(BotId) ->
	state(BotId, 1000).
state(BotId, Timeout) ->
	pid(BotId) ! {state, self()},
	receive {state, State} -> State after Timeout -> {error, timeout} end.

-spec pid(atom() | pid()) -> pid().
pid(Pid) when is_pid(Pid) ->
	Pid;
pid(BotId) ->
	Children = supervisor:which_children(ebridgebot_sup),
	case lists:keyfind(BotId, 1, Children) of
		{_, Pid, _, _} -> Pid;
		_ -> {error, bot_not_found}
	end.

filter_pred(#{component := Component, nick := ComponentNick}) -> %% filter echo messages
	fun(#xmlel{} = Pkt) ->
			case xmpp:decode(Pkt) of
				#message{type = groupchat, to = #jid{server = Component}, from = #jid{resource = ComponentNick}} -> false;
				_ -> true
			end;
		(_) -> true
	end.