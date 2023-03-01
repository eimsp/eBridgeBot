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
%%    ebridgebot:setup(),
    Bots = application:get_env(ebridgebot, bots, []),
    Ids = [binary_to_atom(ComponentName) || {ComponentName, _} <- Bots],
    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1},

    ChildSpecs = [
        #{id => Id,
            start => {ebridgebot, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker}
    || Id <- Ids],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
