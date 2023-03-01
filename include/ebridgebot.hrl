-include_lib("escalus/include/escalus.hrl").

-record(ebridgebot_xmpp, {
	jid_bot :: {{binary(), binary()}, binary()}, %% jid_bot is {<jid>, <bot_name>}
	password :: binary(),
	nick :: binary(),
	type = tg :: atom()}).
-record(ebridgebot_muc, {
	group,
	muc_room :: binary(),
	bot_name :: binary()}).
