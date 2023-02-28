-module(ebridgebot).

%% API
-export([setup/0]).

-include("ebridgebot.hrl").

setup() ->
	application:start(mnesia),
	mnesia:create_table(ebridgebot_xmpp,
		[{attributes, record_info(fields, ebridgebot_xmpp)},
			{disc_copies, [node()]}]),
	mnesia:create_table(ebridgebot_muc,
		[{attributes, record_info(fields, ebridgebot_muc)},
			{disc_copies, [node()]}]).