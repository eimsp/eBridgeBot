-module(ebridgebot_tg_component).
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").

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
	[BotId, BotName, BotToken, Component, Nick, Token] = [proplists:get_value(K, Args) ||
		K <- [bot_id, name, token, component, nick, token]], %% TODO unused params need remove in future
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit=>100, timeout=>60}),
	{ok, #tg_state{bot_id = BotId, bot_name = BotName, component = Component, nick = Nick, token = Token}}.

%% Function that handles information message received from the group chat of Telegram
handle_info({pe4kin_update, BotName,
		#{<<"message">> := #{<<"chat">> := #{<<"id">> := _Id, <<"type">> := <<"group">>}}} = TgMsg},
			_Client, #tg_state{bot_name = BotName} = State) ->
	ct:print("tg msg: ~p", [TgMsg]),
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
handle_info({state, Pid}, _Client, State) ->
	Pid ! {state, State},
	{ok, State};
handle_info(Info, _Client, State) ->
	ct:print("handle component: ~p", [Info]),
	{ok, State}.

%% Function that processes the Stanza from the Client with the
%% UserState state of the user
process_stanza(Stanza, Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	ct:print("handle component stanza: ~p", [Stanza]),
	{ok, State}.

terminate(Reason, State) ->
	ct:print("!#!terminate/2 ~p", [{Reason, State}]),
	component_stopped.

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