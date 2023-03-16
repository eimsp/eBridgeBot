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
			[{component, <<"tg.localhost">>}, %% XMPP component host
				{name, <<"ebridge_bot">>}, %% 
				{host, <<"127.0.0.1">>},
				{nick, <<"tg_bot">>},
				{password, <<"secret">>},
				{module, ebridgebot_tg},
				{port, 8888},
				{token, <<"6066841531:AAEK0aUdaP6eoJWcS0020VOyYQpNhhMpBPE">>},
				{linked_rooms, [{-930545885, <<"deribit.test2@conference.localhost">>}]}]}]}]}
```

[comment]: <> (* `bots` - list of bots)

[comment]: <> (* ` bots/<bot_id>` - `<bot_id>` unique bot id)

[comment]: <> (* ebridgebot_xmpp_host - the host of the XMPP server to connect to)

[comment]: <> (* ebridgebot_xmpp_port - the port of the XMPP server to connect to)

[comment]: <> (* ebridgebot_xmpp_username - the username to use for XMPP authentication)

[comment]: <> (* ebridgebot_xmpp_password - the password to use for XMPP authentication)

[comment]: <> (* ebridgebot_telegram_bot_token - the token of the Telegram bot to use for communication)

* `throttle:setup(Scope, RateLimit, RatePeriod)`: setup a rate limit
   for a given `Scope`, allowing at most `RateLimit` requests per
   `RatePeriod`. Allowed rate periods are `per_second`, `per_minute`,
   `per_hour` and `per_day`.

   Rates can also be set via application environment instead of
   calling `setup`:

   ```erlang
   {throttle, [{rates, [{my_global_scope, 10, per_second}
                        {my_expensive_endpoint, 2, per_minute}]}]}
   ```

* `throttle:check(Scope, Key)`: attempt to request `Scope` with a
  given `Key` (e.g. user token, IP). The result will be `{ok,
  RemainingAttempts, TimeToReset}` if there are attempts left or
  `{limit_exceeded, 0, TimeToReset}` if there aren't.

* `throttle:peek(Scope, Key)`: returns the same result as `check`
  without increasing the requests count.

### Distributed support

By default, throttle keeps the attempt counters on ETS tables, and
therefore those are local to the Erlang node. Mnesia can be used
instead to enfore access limits across all connected nodes, by setting
the `driver` configuration parameter to `throttle_mnesia`:

``` erlang
{throttle, [{driver, throttle_mnesia},
            {rates, [{my_global_scope, 10, per_second}]}]}
```

When using the Mnesia driver, `throttle_mnesia:setup()` needs to be
called after the cluster is connected (the tables have to be shared across
nodes, so the nodes must be visible before intialization):

``` erlang
(n1@127.0.0.1)1> application:set_env(throttle, driver, throttle_mnesia).
ok
(n1@127.0.0.1)2> application:ensure_all_started(throttle).
{ok,[throttle]}
(n1@127.0.0.1)3> net_kernel:connect('n2@127.0.0.1').
true
(n1@127.0.0.1)4> throttle_mnesia:setup().
ok
```

When checking for a Key to access a given Scope, an access counter is
incremented in Mnesia. The
[activity access context](http://learnyousomeerlang.com/mnesia#access-and-context)
for that operation can be configured with the `access_context`
parameter:

``` erlang
{throttle, [{driver, throttle_mnesia},
            {access_context, sync_transaction}]}.
```

By default, the `async_dirty` context is used, which prioritizes speed
over consistency when propagating the counter increment. This means
there's a chance of two nodes getting access to a resource when there
is one attempt left. Depending the application, it may make more
sense to choose a different context (like `sync_transaction`) to
reduce the chances of allowing accesses above the limit.

## Examples

### Shell
``` erlang
1> application:ensure_all_started(throttle).
{ok,[throttle]}
2> throttle:setup(my_api_endpoint, 3, per_minute).
ok
3> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,2,30362}
4> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,1,29114}
5> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,0,27978}
6> throttle:check(my_api_endpoint, my_token_or_ip).
{limit_exceeded,0,26722}
```

### Cowboy 2.0 limit by IP

Middleware module:

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {{IP, _}, Req2} = cowboy_req:peer(Req),

  case throttle:check(my_api_rate, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded api limit", [IP]),
      Req3 = cowboy_req:reply(429, Req2),
      {stop, Req3};
    _ ->
      {ok, Req2, Env}
  end.
```

Using it:

``` erlang
cowboy:start_clear(my_http_listener, [{port, 8080}], #{
		env => #{dispatch => Dispatch},
		middlewares => [cowboy_router, throttling_middleware, cowboy_handler]
	}),
```

### Cowboy 2.0 limit by Authorization header

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  Authorization = cowboy_req:header(<<"authorization">>, Req),

  case throttle:check(my_api_rate, Authorization) of
    {limit_exceeded, _, _} ->
      lager:warning("Auth ~p exceeded api limit", [Authorization]),
      Req3 = cowboy_req:reply(429, Req),
      {stop, Req2};
    _ ->
      {ok, Req, Env}
  end.
```

Note that assumes all requests have an authorization header. A more
realistic approach would be to fallback to an IP limit when
Authorization is not present.

### Cowboy 1.0 limit by IP

Middleware module:

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {{IP, _}, Req2} = cowboy_req:peer(Req),

  case throttle:check(my_api_rate, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded api limit", [IP]),
      {error, 429, Req2};
    _ ->
      {ok, Req2, Env}
  end.
```

Using it:

``` erlang
cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                    [{env, [{dispatch, Dispatch}]},
                     {middlewares, [cowboy_router, throttling_middleware, cowboy_handler]}]
                   ),
```

A more detailed example, choosing the rate based on the path, can be found [here](https://github.com/lambdaclass/holiday_ping/blob/26a3d83faaad6977c936a40fe273cd45954d9259/src/throttling_middleware.erl).
