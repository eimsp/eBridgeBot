%%%-------------------------------------------------------------------
%%% @author cryoflamer
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2023 2:12 PM
%%%-------------------------------------------------------------------
-author("cryoflamer").

-record(tg_id, {
	chat_id = [] :: neg_integer(),
	id = [] :: integer()}).

-record(tg_state, {
	bot_id = [] :: atom(),
	bot_pid = [] :: pid(),
	bot_name = [] :: binary(),
	component = [] :: binary(),
	nick = [] :: binary(),
	token = [] :: binary(),
	rooms = [] :: list(),
	context = [] :: any()}).
