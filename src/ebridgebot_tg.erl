-module(ebridgebot_tg).

%% API
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").
-include("ebridgebot_tg.hrl").

init(Args) ->
	application:ensure_all_started(pe4kin),
	[BotName, BotToken] = [proplists:get_value(K, Args) || K <- [name, token]],
	pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
	pe4kin_receiver:subscribe(BotName, self()),
	pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
%%	NewRooms = [#muc_state{group_id = TgId, muc_jid = MucJid} || {TgId, MucJid} <- Rooms],
	self() ! enter_linked_rooms, %% enter to all linked rooms

%%	application:start(mnesia),
%%	mnesia:create_table(ebridgebot_component:bot_table(BotId),
%%		[{attributes, record_info(fields, xmpp_link)},
%%			{index, [xmpp_id, uid]},
%%			{disc_copies, [node()]}]),

	{ok, #{token => BotToken}}.
