 # eBridgeBot
Erlang based platform for bridges between a growing number of protocols.
eBridgeBot is an OTP application implemented in Erlang 
that integrates XMPP components through API with third-party messengers. 

[comment]: <> (At this stage, with eBridgeBot you can receive, send, edit and delete )

[comment]: <> (messages using [telegram bot API]&#40;https://core.telegram.org/bots/api&#41;.)

[comment]: <> (## Installation)

[comment]: <> (# throttle)

[comment]: <> ([![Hex.pm]&#40;https://img.shields.io/hexpm/v/lambda_throttle.svg&#41;]&#40;https://hex.pm/packages/lambda_throttle&#41;)

[comment]: <> ([![Build Status]&#40;https://travis-ci.org/lambdaclass/throttle.svg?branch=master&#41;]&#40;https://travis-ci.org/lambdaclass/throttle&#41;)

[comment]: <> ([![Coverage Status]&#40;https://coveralls.io/repos/github/lambdaclass/throttle/badge.svg?branch=master&#41;]&#40;https://coveralls.io/github/lambdaclass/throttle?branch=master&#41;)

[comment]: <> (An OTP application to implement throttling/rate limiting of resources.)

## Rebar3 dependency

[comment]: <> (```erl)

[comment]: <> ({throttle, "0.3.0", {pkg, lambda_throttle}})

[comment]: <> (```)

## Build

    $ rebar3 compile

## Usage

[comment]: <> (The application allows to limit different resources &#40;scopes&#41; at different rates.)


The application allows you to link jabber group 
chats with group chats of third-party messengers. At this stage, 
communication with Telegram is implemented through its [bot API](https://core.telegram.org/bots/api)

The application can be configured by modifying the `etc/sys.config` file. The following configuration parameters are available:

```erlang
% First of all, you need to get bot's credentials.
% Consult telegram docs https://core.telegram.org/bots#6-botfather

[{ebridgebot,
	 {bots, %% list of bots
		[{tg_bot, %% unique id of the bot
			[{component, <<"tg.localhost">>}, %% XMPP component host name
             {port, 8888}, %% XMPP service port
             {password, <<"secret">>}, %% XMPP component password
             {name, <<"botname">>}, %% The telegram bot name
             {host, <<"127.0.0.1">>}, %% XMPP server address
             {nick, <<"tg_bot">>}, %% XMPP nick
             {module, ebridgebot_tg}, %% implement Telegram callbacks or other third party messenger
             {token, <<"Telegram-token">>}, %% 
            {linked_rooms, [{-930545885, <<"my.room@conference.localhost">>}]} %% link Telegram chat id with XMPP MUC chat
            ]}]}]
```

[comment]: <> (* `bots` - list of bots)

[comment]: <> (* ` bots/<bot_id>` - `<bot_id>` unique bot id)

[comment]: <> (* ebridgebot_xmpp_host - the host of the XMPP server to connect to)

[comment]: <> (* ebridgebot_xmpp_port - the port of the XMPP server to connect to)

[comment]: <> (* ebridgebot_xmpp_username - the username to use for XMPP authentication)

[comment]: <> (* ebridgebot_xmpp_password - the password to use for XMPP authentication)

[comment]: <> (* ebridgebot_telegram_bot_token - the token of the Telegram bot to use for communication)

[comment]: <> (* `throttle:setup&#40;Scope, RateLimit, RatePeriod&#41;`: setup a rate limit)

[comment]: <> (   for a given `Scope`, allowing at most `RateLimit` requests per)

[comment]: <> (   `RatePeriod`. Allowed rate periods are `per_second`, `per_minute`,)

[comment]: <> (   `per_hour` and `per_day`.)

[comment]: <> (   Rates can also be set via application environment instead of)

[comment]: <> (   calling `setup`:)

[comment]: <> (   ```erlang)

[comment]: <> (   {throttle, [{rates, [{my_global_scope, 10, per_second})

[comment]: <> (                        {my_expensive_endpoint, 2, per_minute}]}]})

[comment]: <> (   ```)

[comment]: <> (* `throttle:check&#40;Scope, Key&#41;`: attempt to request `Scope` with a)

[comment]: <> (  given `Key` &#40;e.g. user token, IP&#41;. The result will be `{ok,)

[comment]: <> (  RemainingAttempts, TimeToReset}` if there are attempts left or)

[comment]: <> (  `{limit_exceeded, 0, TimeToReset}` if there aren't.)

[comment]: <> (* `throttle:peek&#40;Scope, Key&#41;`: returns the same result as `check`)

[comment]: <> (  without increasing the requests count.)

[comment]: <> (### Distributed support)

[comment]: <> (By default, throttle keeps the attempt counters on ETS tables, and)

[comment]: <> (therefore those are local to the Erlang node. Mnesia can be used)

[comment]: <> (instead to enfore access limits across all connected nodes, by setting)

[comment]: <> (the `driver` configuration parameter to `throttle_mnesia`:)

[comment]: <> (``` erlang)

[comment]: <> ({throttle, [{driver, throttle_mnesia},)

[comment]: <> (            {rates, [{my_global_scope, 10, per_second}]}]})

[comment]: <> (```)

[comment]: <> (When using the Mnesia driver, `throttle_mnesia:setup&#40;&#41;` needs to be)

[comment]: <> (called after the cluster is connected &#40;the tables have to be shared across)

[comment]: <> (nodes, so the nodes must be visible before intialization&#41;:)

[comment]: <> (``` erlang)

[comment]: <> (&#40;n1@127.0.0.1&#41;1> application:set_env&#40;throttle, driver, throttle_mnesia&#41;.)

[comment]: <> (ok)

[comment]: <> (&#40;n1@127.0.0.1&#41;2> application:ensure_all_started&#40;throttle&#41;.)

[comment]: <> ({ok,[throttle]})

[comment]: <> (&#40;n1@127.0.0.1&#41;3> net_kernel:connect&#40;'n2@127.0.0.1'&#41;.)

[comment]: <> (true)

[comment]: <> (&#40;n1@127.0.0.1&#41;4> throttle_mnesia:setup&#40;&#41;.)

[comment]: <> (ok)

[comment]: <> (```)

[comment]: <> (When checking for a Key to access a given Scope, an access counter is)

[comment]: <> (incremented in Mnesia. The)

[comment]: <> ([activity access context]&#40;http://learnyousomeerlang.com/mnesia#access-and-context&#41;)

[comment]: <> (for that operation can be configured with the `access_context`)

[comment]: <> (parameter:)

[comment]: <> (``` erlang)

[comment]: <> ({throttle, [{driver, throttle_mnesia},)

[comment]: <> (            {access_context, sync_transaction}]}.)

[comment]: <> (```)

[comment]: <> (By default, the `async_dirty` context is used, which prioritizes speed)

[comment]: <> (over consistency when propagating the counter increment. This means)

[comment]: <> (there's a chance of two nodes getting access to a resource when there)

[comment]: <> (is one attempt left. Depending the application, it may make more)

[comment]: <> (sense to choose a different context &#40;like `sync_transaction`&#41; to)

[comment]: <> (reduce the chances of allowing accesses above the limit.)

## Examples

### Shell
``` erlang
$ rebar3 shell
```
[comment]: <> (2> application:ensure_all_started&#40;ebridgebot&#41;.)

[comment]: <> ({ok,[ebridgebot]})

[comment]: <> (2> throttle:setup&#40;my_api_endpoint, 3, per_minute&#41;.)

[comment]: <> (ok)

[comment]: <> (3> throttle:check&#40;my_api_endpoint, my_token_or_ip&#41;.)

[comment]: <> ({ok,2,30362})

[comment]: <> (4> throttle:check&#40;my_api_endpoint, my_token_or_ip&#41;.)

[comment]: <> ({ok,1,29114})

[comment]: <> (5> throttle:check&#40;my_api_endpoint, my_token_or_ip&#41;.)

[comment]: <> ({ok,0,27978})

[comment]: <> (6> throttle:check&#40;my_api_endpoint, my_token_or_ip&#41;.)

[comment]: <> ({limit_exceeded,0,26722})

[comment]: <> (```)

[comment]: <> (### Cowboy 2.0 limit by IP)

[comment]: <> (Middleware module:)

[comment]: <> (``` erlang)

[comment]: <> (-module&#40;throttling_middleware&#41;.)

[comment]: <> (-behavior&#40;cowboy_middleware&#41;.)

[comment]: <> (-export&#40;[execute/2]&#41;.)

[comment]: <> (execute&#40;Req, Env&#41; ->)

[comment]: <> (  {{IP, _}, Req2} = cowboy_req:peer&#40;Req&#41;,)

[comment]: <> (  case throttle:check&#40;my_api_rate, IP&#41; of)

[comment]: <> (    {limit_exceeded, _, _} ->)

[comment]: <> (      lager:warning&#40;"IP ~p exceeded api limit", [IP]&#41;,)

[comment]: <> (      Req3 = cowboy_req:reply&#40;429, Req2&#41;,)

[comment]: <> (      {stop, Req3};)

[comment]: <> (    _ ->)

[comment]: <> (      {ok, Req2, Env})

[comment]: <> (  end.)

[comment]: <> (```)

[comment]: <> (Using it:)

[comment]: <> (``` erlang)

[comment]: <> (cowboy:start_clear&#40;my_http_listener, [{port, 8080}], #{)

[comment]: <> (		env => #{dispatch => Dispatch},)

[comment]: <> (		middlewares => [cowboy_router, throttling_middleware, cowboy_handler])

[comment]: <> (	}&#41;,)

[comment]: <> (```)

[comment]: <> (### Cowboy 2.0 limit by Authorization header)

[comment]: <> (``` erlang)

[comment]: <> (-module&#40;throttling_middleware&#41;.)

[comment]: <> (-behavior&#40;cowboy_middleware&#41;.)

[comment]: <> (-export&#40;[execute/2]&#41;.)

[comment]: <> (execute&#40;Req, Env&#41; ->)

[comment]: <> (  Authorization = cowboy_req:header&#40;<<"authorization">>, Req&#41;,)

[comment]: <> (  case throttle:check&#40;my_api_rate, Authorization&#41; of)

[comment]: <> (    {limit_exceeded, _, _} ->)

[comment]: <> (      lager:warning&#40;"Auth ~p exceeded api limit", [Authorization]&#41;,)

[comment]: <> (      Req3 = cowboy_req:reply&#40;429, Req&#41;,)

[comment]: <> (      {stop, Req2};)

[comment]: <> (    _ ->)

[comment]: <> (      {ok, Req, Env})

[comment]: <> (  end.)

[comment]: <> (```)

[comment]: <> (Note that assumes all requests have an authorization header. A more)

[comment]: <> (realistic approach would be to fallback to an IP limit when)

[comment]: <> (Authorization is not present.)

[comment]: <> (### Cowboy 1.0 limit by IP)

[comment]: <> (Middleware module:)

[comment]: <> (``` erlang)

[comment]: <> (-module&#40;throttling_middleware&#41;.)

[comment]: <> (-behavior&#40;cowboy_middleware&#41;.)

[comment]: <> (-export&#40;[execute/2]&#41;.)

[comment]: <> (execute&#40;Req, Env&#41; ->)

[comment]: <> (  {{IP, _}, Req2} = cowboy_req:peer&#40;Req&#41;,)

[comment]: <> (  case throttle:check&#40;my_api_rate, IP&#41; of)

[comment]: <> (    {limit_exceeded, _, _} ->)

[comment]: <> (      lager:warning&#40;"IP ~p exceeded api limit", [IP]&#41;,)

[comment]: <> (      {error, 429, Req2};)

[comment]: <> (    _ ->)

[comment]: <> (      {ok, Req2, Env})

[comment]: <> (  end.)

[comment]: <> (```)

[comment]: <> (Using it:)

[comment]: <> (``` erlang)

[comment]: <> (cowboy:start_http&#40;my_http_listener, 100, [{port, 8080}],)

[comment]: <> (                    [{env, [{dispatch, Dispatch}]},)

[comment]: <> (                     {middlewares, [cowboy_router, throttling_middleware, cowboy_handler]}])

[comment]: <> (                   &#41;,)

[comment]: <> (```)

[comment]: <> (A more detailed example, choosing the rate based on the path, can be found [here]&#40;https://github.com/lambdaclass/holiday_ping/blob/26a3d83faaad6977c936a40fe273cd45954d9259/src/throttling_middleware.erl&#41;.)
