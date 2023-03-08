-define(dbg(Fmt, Args),
	case xmpp_config:debug(global) of
		{ok, true} -> error_logger:info_msg(Fmt, Args);
		_ -> false
	end).

-record(xmpp_link, {
	time = erlang:system_time(microsecond) :: integer(),
	xmpp_id = [] :: binary(),
	uid = [] :: any()}).