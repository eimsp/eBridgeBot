-module(ebridgebot_component).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").

%% API
-export([start/1, start_link/1, init/1, handle_info/3, process_stanza/3, process_stanza/2, process_stanza/1, terminate/2, stop/1,
	state/1, state/2, pid/1, filter_pred/1]).

start(#{bot_id := BotId} = Args) ->
	escalus_component:start({local, BotId}, ebridgebot_component, maps:to_list(Args), Args).

start_link(Args) ->
	escalus_component:start_link(?MODULE, maps:to_list(Args), Args).

init(Args) ->
	?dbg("~p: init with args: ~p", [?MODULE, Args]),
	DefaultState = #{rooms => [], upload_host => <<"upload.localhost">>, format => #{}},
	State = #{rooms := Rooms, bot_id := BotId, module := Module} = maps:merge(DefaultState, Args),
	LinkedRooms = [#muc_state{group_id = TgId, muc_jid = string:lowercase(MucJid), password = case MucMap of #{password := P} -> P; _-> undefined end}
		|| {TgId, #{jid := MucJid} = MucMap} <- Rooms],
	application:start(mnesia),
	mnesia:create_table(ebridgebot:bot_table(BotId),
		[{attributes, record_info(fields, xmpp_link)},
			{index, [origin_id, mam_id, uid]},
			{disc_copies, [node()]}]),

	self() ! {linked_rooms, presence, available}, %% enter to all linked rooms

	Module:init(State#{rooms => LinkedRooms, upload => #{}}).

handle_info({link_scheduler, ClearInterval, LifeSpan} = Info, _Client, State) -> %% ClearInterval and LifeSpan in milliseconds
	?dbg("handle: ~p", [Info]),
	catch erlang:cancel_timer(maps:get(link_scheduler_ref, State)),
	handle_info({remove_old_links, erlang:system_time(microsecond) - LifeSpan * 1000}, _Client, State),
	{ok, TRef} = timer:send_after(ClearInterval, Info),
	{ok, State#{link_scheduler_ref => TRef}};
handle_info(stop_link_scheduler, _Client, #{link_scheduler_ref := TRef} = State) ->
	?dbg("handle: ~p", [stop_link_scheduler]),
	catch erlang:cancel_timer(TRef),
	{ok, maps:remove(link_scheduler_ref, State)};
handle_info({remove_old_links, OldestTS} = Info, _Client, #{bot_id := BotId} = State) ->
	?dbg("handle: ~p", [Info]),
	MatchSpec = [{{Table = ebridgebot:bot_table(BotId), '$1', '_', '_', '_'}, [{'<', '$1', OldestTS}], ['$1']}],
	spawn(fun() -> [mnesia:dirty_delete(Table, K) || K <- mnesia:dirty_select(Table, MatchSpec)] end),
	{ok, State};

handle_info({add_room, ChatId, MucJid}, Client, State) ->
	handle_info({add_room, ChatId, MucJid, undefined}, Client, State);
handle_info({add_room, ChatId, MucJid, Password} = Info, _Client, #{rooms := Rooms} = State) ->
	?dbg("handle: ~p", [Info]),
	LMucJid = string:lowercase(MucJid),
	LinkedRooms =
		case [MucState#muc_state{password = Password} ||
				#muc_state{group_id = ChatId2, muc_jid = J} = MucState <- Rooms, ChatId == ChatId2, LMucJid == J] of
			[] -> [#muc_state{group_id = ChatId, muc_jid = LMucJid, password = Password} | Rooms];
			Rooms2 -> Rooms2
		end,
	{ok, State#{rooms => LinkedRooms}};
handle_info({presence, #{type := Type, jid := MucJid} = MucMap} = Info, Client, #{component := Component, nick := Nick} = State)
	when Type == available; Type == unavailable ->
	?dbg("handle: ~p", [Info]),
	Nick2 = case MucMap of #{nick := N} -> N; _ -> Nick end,
	EnterPresence = #presence{type = Type, from = jid:make(Component), to = jid:replace_resource(jid:decode(MucJid), Nick2),
		sub_els = [#muc{password = ebridgebot:password(MucMap)}]},
	escalus:send(Client, xmpp:encode(EnterPresence)),
	{ok, State};
handle_info({event, #{type := SubAction, jid := MucJid} = MucMap} = Info, Client, #{component := Component, nick := Nick} = State)
	when SubAction == subscribe; SubAction == unsubscribe ->
	?dbg("handle: ~p", [Info]),
	escalus:send(Client, xmpp:encode(ebridgebot:iq(SubAction, jid:make(Component), jid:decode(MucJid), Nick, ebridgebot:password(MucMap)))),
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
					false -> handle_info({Type, #{type => Action, jid => MucJid}}, Client, State)
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
	#{FileId := #upload_info{content_type = ContentType, file_path = FilePath, muc_jids = RoomJids, uid = Uid, packet_fun = PktFun}} = Upload,
	?dbg("upload slot: ~p", [IQ]),
	Pid = self(),
	spawn( %% async get and put data
		fun() ->
			try %% TODO error handling
				{ok, Data} = Module:get_file(State#{file_path => FilePath}), %% TODO set get_file more universal
				{ok, {{"HTTP/1.1", 201, _}, _, _}} =
					httpc:request(put, {binary_to_list(PutURL), [], binary_to_list(ContentType), Data}, [], []),
				PktFun2 = ebridgebot:fold_pkt_fun(PktFun, [{text, GetURL}, {tag, #message_upload{body = [#message_upload_body{url = GetURL}]}}]),
				[ebridgebot:send_to(Pid, PktFun2, MucJid, BotId, Uid)
					|| MucJid <- RoomJids]
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
process_stanza(#message{id = Id, from = #jid{resource = Nick}} = Pkt, Client, #{} = State) ->
	Pkt2 =
		case xmpp:get_subtag(Pkt, #origin_id{}) of
			false -> xmpp:set_subtag(Pkt, #origin_id{id = Id});
			_ -> Pkt
		end,
	stanza_decorator([#ps_event{}, #message_entities{}, #bot{}, #reply{}, #replace{}, #fasten_apply_to{}, #origin_id{}],
		[Pkt2, State#{usernick => Nick, entities => [], send_fun => send_message}, Client]),
	{ok, State};
process_stanza(Stanza, _Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	?dbg("handle component stanza: ~p", [Stanza]),
	{ok, State}.

%% callbacks for ebridgebot:tag_decorator
process_stanza(#ps_event{items = #ps_items{node = ?NS_MUCSUB_NODES_MESSAGES, items = [#ps_item{sub_els = [#xmlel{name = <<"message">>} = Pkt]}]}},
	[#message{type = normal, from = #jid{resource = <<>>}, to = #jid{resource = <<>>}}, #{} = State, Client]) -> %% event message if subscribed
	?dbg("handle event sub message: ~p", [Pkt]),
	process_stanza(Pkt, Client, State);
process_stanza(#ps_event{} = Event,	[#message{} = _Pkt, #{} = State | _]) ->
	?dbg("handle event message: ~p", [Event]),
	%% TODO not implemented
	{ok, State};
process_stanza(#origin_id{id = OriginId}, [#message{type = groupchat, body = [#text{data = Text}]} = Pkt, #{bot_id := BotId} = State | TState]) ->
	case ebridgebot:upd_links(BotId, OriginId, xmpp:get_subtag(Pkt, #mam_archived{})) of
		[_ | _] -> {ok, State}; %% not send to third party client if messages already linked
		[] -> process_stanza([Pkt, State#{text => Text} | TState])
	end;
process_stanza(#reply{id = ReplyToId}, [Pkt = #message{body = [#text{data = Text}]}, #{bot_id := BotId} = State | TState]) -> %% reply message from xmpp groupchat
	?dbg("reply: ~p", [Pkt]),
	{NewState, Tags} =
		case ebridgebot:index_read(BotId, ReplyToId, #xmpp_link.origin_id) of
			[#xmpp_link{uid = Uid} | _] ->
				{State#{reply_to => Uid}, [#feature_fallback{}]};
			_ -> {State, []}
		end,
	stanza_decorator(Tags ++ [#replace{}, #origin_id{}], [Pkt, NewState#{text => Text} | TState]);
process_stanza(#feature_fallback{body = #feature_fallback_body{start = Start, 'end' = End}},
	[Pkt = #message{body = [#text{data = Text}]}, #{reply_to := _Uid} = State | TState]) -> %% fallback reply message from xmpp groupchat
	?dbg("fallback: ~p", [Pkt]),
	Text2 = case xmpp:get_subtag(Pkt, #feature_fallback{}) of
		        #feature_fallback{body = #feature_fallback_body{start = Start, 'end' = End}} ->
			        iolist_to_binary([binary:part(Text, Pos, Len) || {Pos, Len} <- [{0, Start}, {End, byte_size(Text) - End}]]);
		        _ -> Text
	        end,
	stanza_decorator([#replace{}, #origin_id{}], [Pkt#message{body = [#text{data = Text2}]}, State#{text => Text2} | TState]);
process_stanza(#bot{}, [Pkt, #{format := #{system := Type} = Format} = State | TState]) -> %% bot message format from xmpp groupchat
	?dbg("bot format: ~p", [Pkt]),
	stanza_decorator([#reply{}, #replace{}, #fasten_apply_to{}, #origin_id{}],
		[Pkt, State#{format => Format#{text => Type}, usernick => <<>>} | TState]);
process_stanza(#bot{}, State) -> %% bot message from xmpp groupchat
	?dbg("bot: ~p", [State]),
	stanza_decorator([#reply{}, #replace{}, #fasten_apply_to{}, #origin_id{}], State);
process_stanza(#message_entities{items = Entities}, [Pkt, State | TState]) -> %% entities from xmpp groupchat
	?dbg("entities: ~p", [Pkt]),
	NewState = State#{entities => [#{type => T, offset => Offset, length => Length}
		|| #message_entity{type = T, offset = Offset, length = Length} <- Entities]},
	stanza_decorator([#bot{}, #reply{}, #replace{}, #fasten_apply_to{}, #origin_id{}], [Pkt, NewState | TState]);
process_stanza(#replace{}, [{uid, Uid}, #message{type = groupchat, body = [#text{data = Text}]} = Pkt,
		#{bot_id := BotId, module := Module} = State | _]) -> %% edit message from xmpp groupchat with uid
	?dbg("replace: ~p", [Pkt]),
	Module:edit_message(State#{uid => Uid, text => Text}),
	ebridgebot:write_link(BotId, xmpp:get_subtag(Pkt, #origin_id{}), Uid, xmpp:get_subtag(Pkt, #mam_archived{})),
	{ok, State};
%% old message_retract eq retract_id
process_stanza(#fasten_apply_to{sub_els = [#message_moderated_21{retract = #retract_id{}}]}, [{uid, _Uid}, Pkt | _] = Args) -> %% retract message from groupchat by moderator
	?dbg("moderator retract: ~p", [Pkt]),
	process_stanza(#fasten_apply_to{sub_els = [#retract_id{}]}, Args);
process_stanza(#fasten_apply_to{sub_els = [#retract_id{}]}, [{uid, Uid}, #message{type = groupchat} = Pkt,
	#{bot_id := BotId, module := Module} = State | _]) -> %% retract message from xmpp groupchat
	?dbg("retract: ~p", [Pkt]),
	Module:delete_message(State#{uid => Uid}),
	Table = ebridgebot:bot_table(BotId),
	[mnesia:dirty_delete(Table, TimeId) || #xmpp_link{time = TimeId} <- ebridgebot:index_read(BotId, Uid, #xmpp_link.uid)],
	{ok, State};
process_stanza(Tag, [#message{type = groupchat, from = #jid{} = From} = Pkt, #{bot_id := BotId, rooms := Rooms, module := Module} = State | TState] = Args)
	when is_record(Tag, replace); is_record(Tag, fasten_apply_to) -> %% edited or moderated message from xmpp groupchat
	?dbg("replace or retract msg to third party client: ~p", [Pkt]),
	MucFrom = jid:encode(jid:remove_resource(From)),
	Id = element(#fasten_apply_to.id = #replace.id, Tag), %% #fasten_apply_to.id == #replace.id
	Attr = case Tag of #fasten_apply_to{sub_els = [#message_moderated_21{}]} -> #xmpp_link.mam_id; _ -> #xmpp_link.origin_id end,
	Links = ebridgebot:index_read(BotId, Id, Attr),
	[case lists:filter(Module:link_pred(State#{group_id => ChatId}), Links) of
		 [#xmpp_link{uid = Uid} | _] ->
			 process_stanza(Tag, [{uid, Uid} | Args]);
		 [] when is_record(Tag, replace) ->
			 #message{body = [#text{data = Text}]} = Pkt,
			 process_stanza(xmpp:get_subtag(Pkt, #origin_id{}), [Pkt, State#{text => Text} | TState]); %% send new message if the replaced message does not exist
		 [] -> ok
	 end || #muc_state{muc_jid = MucJid, group_id = ChatId} <- Rooms, MucFrom == MucJid],
	{ok, State};
process_stanza(_, [#message{} = Pkt | _] = StateList) ->
	?dbg("msg without origin id: ~p", [Pkt]),
	process_stanza(StateList).

process_stanza([#message{type = groupchat, from = #jid{resource = Nick}} = Pkt,	#{upload_endpoint := UploadEndpoint, text := Text} = State | TState]) ->
	?dbg("upload to third party client: ~p", [Pkt]),
	NewState =
		case binary:match(Text, [UploadEndpoint]) of
			nomatch -> State;
			_ -> %% TODO if endpoint path has port then tg does not allow upload
				State#{send_fun => send_data,
						mime => hd(mimetypes:filename(Text)),
						file_uri => Text,
						caption => <<Nick/binary, ":">>}
		end,
	process_stanza([Pkt, maps:remove(upload_endpoint, NewState) | TState]);
process_stanza([#message{type = groupchat, from = From} = Pkt,
	#{bot_id := BotId, rooms := Rooms, module := Module, send_fun := SendFun, text := _} = State | _]) ->
	MucFrom = jid:encode(jid:remove_resource(From)),
	?dbg("send to third party client: ~p", [Pkt]),
	[OriginTag, MamArchivedTag] = [xmpp:get_subtag(Pkt, Tag) || Tag <- [#origin_id{}, #mam_archived{}]],
	[case Module:SendFun(State#{chat_id => ChatId}) of
		 {ok, Uid} ->
			 ebridgebot:write_link(BotId, OriginTag, Uid, MamArchivedTag);
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

stanza_decorator(Tags, S) ->
	(ebridgebot:tag_decorator(Tags, S, ?MODULE, process_stanza))().