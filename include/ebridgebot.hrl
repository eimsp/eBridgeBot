-record(xmpp_link, {
	time = erlang:system_time(microsecond) :: integer(),
	xmpp_id = [] :: binary(),
	uid = [] :: any()}).