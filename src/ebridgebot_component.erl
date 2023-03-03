-module(ebridgebot_component).
-export([init/1,
	handle_info/3,
	process_stanza/3, terminate/2]).

init(Args) ->
	ct:print("!!init(~p)", [Args]),
	{ok, []}.

%% Function that handles information message Info received
%% from the Client in the state State
handle_info(Info, Client, State) ->
	%% Here you can implement the handling of the information message
	%% and change the State accordingly
	ct:print("!!handle_info(~p, ~p, ~p)", [Info, Client, State]),
	{ok, State}.

%% Function that processes the Stanza from the Client with the
%% UserState state of the user
process_stanza(Stanza, Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	ct:print("!@!process_stanza(~p, ~p, ~p)", [Stanza, Client, State]),
	{ok, State}.

terminate(Reason, State) ->
	ct:print("!#!terminate/2 ~p", [{Reason, State}]),
	component_stopped.