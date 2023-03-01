-module(ebridgebot_component).
-export([handle_info/3, process_stanza/3]).

%% Function that handles information message Info received
%% from the Client in the state State
handle_info(Info, Client, State) ->
	%% Here you can implement the handling of the information message
	%% and change the State accordingly
	{noreply, State}.

%% Function that processes the Stanza from the Client with the
%% UserState state of the user
process_stanza(Stanza, Client, UserState) ->
	%% Here you can implement the processing of the Stanza and
	%% change the UserState accordingly
	{noreply, UserState}.