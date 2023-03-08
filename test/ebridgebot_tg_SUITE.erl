%%%-------------------------------------------------------------------
%%% @author cryoflamer
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2023 11:35 AM
%%%-------------------------------------------------------------------
-module(ebridgebot_tg_SUITE).
-author("cryoflamer").
-compile(export_all).
%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("xmpp/include/xmpp_codec.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

-import(ebridgebot_component_SUITE, [get_property/2, enter_room/3]).
-import(ebridgebot, [wait_for_result/2, wait_for_result/4, wait_for_list/1, wait_for_list/2]).

-define(BOT_ID, test_tg_bot).
-define(NICK, atom_to_binary(?BOT_ID)).

all() ->
	[{group, main}].

groups() ->
	MainStories = [muc_story],
	[{main, [sequence], MainStories}, {local, [sequence], MainStories}].

init_per_suite(Config) ->
	[escalus:Fun([{escalus_user_db, {module, escalus_ejabberd}} | Config], escalus_users:get_users([alice])) || Fun <- [delete_users, create_users]],
	application:stop(ebridgebot),
	Args = [{bot_id, ?BOT_ID},
		{component, escalus_ct:get_config(ejabberd_service)}, %% TODO move to test.config
		{name, <<"ebridge_bot">>}, %% TODO change for test name and insert meck
		{host, escalus_ct:get_config(ejabberd_addr)},
		{nick, ?NICK},
		{password, escalus_ct:get_config(ejabberd_service_password)},
		{module, ebridgebot_tg_component}, %% TODO set ebridgebot_tg module in future
		{port, escalus_ct:get_config(ejabberd_service_port)},
		{token, <<"6066841531:AAEK0aUdaP6eoJWcS0020VOyYQpNhhMpBPE">>}, %% TODO set fake token in future by meck
		{linked_rooms, []}],
	{ok, Pid} = escalus_component:start({local, get_property(bot_id, Args)}, get_property(module, Args), Args, Args),
	Args ++ [{component_pid, Pid} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
	ok = ebridgebot_tg_component:stop(get_property(component_pid, Config)),
	application:start(ebridgebot),
	escalus:end_per_suite(Config).

%%init_per_testcase(test_msg, Config) ->
%%	meck:new(ebridgebot_tg_component),
%%%%	meck:expect(ebridgebot_tg_component, process_stanza, fun process_stanza/3),
%%	escalus:init_per_testcase(test_msg, Config);
%%init_per_testcase(muc_story, Config) ->
%%	ebridgebot_component_SUITE:init_per_testcase(CaseName, Config).
init_per_testcase(CaseName, Config) ->
	ebridgebot_component_SUITE:init_per_testcase(CaseName, Config).


%%end_per_testcase(test_msg, Config) ->
%%	meck:unload(ebridgebot_tg_component),
%%	escalus:end_per_testcase(test_msg, Config);
end_per_testcase(CaseName, Config) ->
%%	catch meck:unload(ebridgebot_tg_component),
	BotId = get_property(bot_id, Config),
	{ok, atomic} = mnesia:delete_table(ebridgebot_tg_component:bot_table(BotId)),
	escalus:end_per_testcase(CaseName, Config).

muc_story(Config) ->
	[RoomNode, ChatId] = [escalus_config:get_ct({ebridgebot_rooms, ebridgebot_test, K}) || K <- [name, chat_id]],
	MucHost = escalus_config:get_ct(muc_host),
	RoomJid = jid:to_string({RoomNode, MucHost, <<>>}),
	AliceNick = escalus_config:get_ct({escalus_users, alice, nick}),
	BotId = get_property(bot_id, Config),
	[Pid] = [get_property(Key, Config) || Key <- [component_pid]],
	#tg_state{bot_id = BotId, rooms = []} = ebridgebot_tg_component:state(Pid),
	escalus:story(Config, [{alice, 1}],
		fun(#client{jid = _AliceJid} = Alice) ->
			enter_room(Alice, RoomJid, AliceNick),
			escalus_client:wait_for_stanzas(Alice, 1),
			Pid ! {link_rooms, ChatId, RoomJid},
			#tg_state{bot_id = BotId, rooms = [#muc_state{group_id = ChatId, state = out}]} = ebridgebot_tg_component:state(Pid),
			Pid ! enter_linked_rooms,
			#tg_state{bot_id = BotId, rooms = [#muc_state{group_id = ChatId, state = pending}]} = ebridgebot_tg_component:state(Pid),
			escalus_client:wait_for_stanzas(Alice, 1),
			#tg_state{bot_id = BotId, rooms = [#muc_state{state = in}]} =
				wait_for_result(fun() -> ebridgebot_tg_component:state(Pid) end,
					fun(#tg_state{rooms = [#muc_state{state = in}]}) -> true; (_) -> false end),
			ok
		end).