-module(ebridgebot).
-behaviour(gen_server).
-compile(export_all).

-include("ebridgebot.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Start the server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Server initialization
init([]) ->
	application:start(mnesia),
	mnesia:create_table(ebridgebot_xmpp,
		[{attributes, record_info(fields, ebridgebot_xmpp)},
			{disc_copies, [node()]}]),
	mnesia:create_table(ebridgebot_muc,
		[{attributes, record_info(fields, ebridgebot_muc)},
			{disc_copies, [node()]}]),
	{ok, []}.

set_bot_info(Jid, Name, Password, Nick, Type) when is_binary(Jid), is_atom(Type) ->
	set_bot_info({escalus_utils:get_username(Jid), escalus_utils:get_server(Jid)}, Name, Password, Nick, Type);
set_bot_info({_, _} = US, Name, Password, Nick, Type) ->
	gen_server:call(?MODULE, #ebridgebot_xmpp{jid_bot = {US, Name}, password = Password, nick = Nick, type = Type}).

% Handle call messages
handle_call(#ebridgebot_xmpp{} = BotData, _From, State) ->
	{reply, mnesia:dirty_write(BotData), State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

% Handle cast messages
handle_cast(_Msg, State) ->
	{noreply, State}.

% Handle other messages
handle_info(_Msg, State) ->
	{noreply, State}.

% Shutdown the server
terminate(_Reason, _State) ->
	ok.

% Handle code changes
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.