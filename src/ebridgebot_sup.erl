%%%-------------------------------------------------------------------
%% @doc ebridgebot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ebridgebot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("ebridgebot.hrl").

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
    application:start(mnesia),
    mnesia:create_table(ebridgebot_muc,
        [{attributes, record_info(fields, ebridgebot_muc)},
            {disc_copies, [node()]}]),

    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1},

    Bots = application:get_env(ebridgebot, bots, []),
    ChildSpecs = [
        #{id => BotId,
            start => {escalus_component, start_link, [ebridgebot_component, Args, Args]},
            restart => permanent,
            shutdown => 5000,
            type => worker}
        || {BotId, Args} <- Bots],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
