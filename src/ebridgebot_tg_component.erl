-module(ebridgebot_tg_component).
-compile(export_all).
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

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotId, BotName, BotToken, Component, Nick, Token] = [proplists:get_value(K, Args) ||
		K <- [bot_id, name, token, component, nick, token]], %% TODO unused params to remove in future
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
	NewRooms = lists:merge([{TgRoomId, MucJid}], Rooms),
	{ok, State#tg_state{rooms = NewRooms}};
handle_info({state, Pid}, _Client, State) ->
	Pid ! {state, State},
	{ok, State};
handle_info(Info, Client, State) ->
	ct:print("!!handle_info(~p, ~p, ~p)", [Info, Client, State]),
	{ok, State}.

%% Function that processes the Stanza from the Client with the
%% UserState state of the user
process_stanza(Stanza, Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	ct:print("!@!process_stanza(~p, ~p, ~p)", [Stanza, Client, State]),
	{ok, State}.

terminate(Reason, State) ->
	ct:print("!#!terminate/2 ~p", [{Reason, State}]),
	component_stopped.

get_state(Pid) when is_pid(Pid) ->
	Pid ! {state, self()},
	receive {state, State} -> State after 1000 -> {error, timeout} end;
get_state(BotId) ->
	get_state(get_pid(BotId)).

get_pid(BotId) ->
	Children = supervisor:which_children(ebridgebot_sup),
	case lists:keyfind(BotId, 1, Children) of
		{_, Pid, _, _} -> Pid;
		_ -> {error, bot_not_found}
	end.