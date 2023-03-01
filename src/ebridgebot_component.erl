-module(ebridgebot_component).
-export([init/1, handle_info/3, process_stanza/3, terminate/2]).

init(_Args) ->
	{ok, []}.

%% Function that handles information message Info received
%% from the Client in the state State
handle_info(Info, Client, State) ->
	%% Here you can implement the handling of the information message
	%% and change the State accordingly
	io:format("~n!!handle_info(~p, ~p, ~p) ~n", [Info, Client, State]),
	{ok, State}.

%% Function that processes the Stanza from the Client with the
%% UserState state of the user
process_stanza(Stanza, Client, State) ->
	%% Here you can implement the processing of the Stanza and
	%% change the State accordingly
	io:format("~n!@!process_stanza(~p, ~p, ~p) ~n", [Stanza, Client, State]),
	{ok, State}.

terminate(Reason, State) ->
	io:format("~n!#!terminate/2 ~p~n", [{Reason, State}]),
	component_stopped.