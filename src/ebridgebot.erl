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

% Handle call messages
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