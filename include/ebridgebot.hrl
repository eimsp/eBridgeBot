-define(dbg(Fmt, Args),
	case xmpp_config:debug(global) of
		{ok, true} -> error_logger:info_msg(Fmt, Args);
		_ -> false
	end).

-define(err(Fmt, Args), error_logger:error_msg(Fmt, Args)).

-record(muc_state, {
	group_id = [] :: any(),
	muc_jid = [] :: binary(),
	state = {out, unsubscribed} :: {out | in | pending, subscribes | unsubscribed}}).

-record(xmpp_link, {
	time = erlang:system_time(microsecond) :: integer(),
	origin_id = [] :: binary(),
	mam_id = [] :: binary(),
	uid = [] :: any()}).

-record(upload_info, {
	file_id = [] :: binary(),
	caption = [] :: binary(),
	nick = [] :: binary(),
	file_path = [] :: binary(),
	content_type = [] :: binary(),
	muc_jids = [] :: binary(),
	uid = [] :: any(),
	send_fun = send :: send | send_edit
}).