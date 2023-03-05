-record(tg_id, {
	chat_id = [] :: neg_integer(),
	id = [] :: integer()}).

-record(xmpp_id, {
	bot_id = [] :: atom(),
	id = [] :: binary()}).

-record(xmpp_link, {
	xmpp_id = [] :: binary(),
	uid = [] :: list(#tg_id{}) | list(),
	time = erlang:system_time(microsecond) :: integer()}).