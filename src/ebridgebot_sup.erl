%%%-------------------------------------------------------------------
%% @doc ebridgebot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ebridgebot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    mnesia:create_schema([node()]),
    DebugInfo = application:get_env(ebridgebot, debug_info, true),
    xmpp:set_config([{debug, DebugInfo}]),
    SupFlags = #{strategy => one_for_one,
        intensity => 1000,
        period => 3600},
    Bots = application:get_env(ebridgebot, bots, []),
    ChildSpecs = [
        #{id => BotId,
            start => {ebridgebot_component, start_link, [BotId, Args]},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [escalus_component]}
        || {BotId, Args} <- Bots],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
