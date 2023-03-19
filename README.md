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

## Examples

### Shell
``` erlang
$ rebar3 shell
```
####start bot component manually

```erlang
$ Args = [{bot_id, test_tg_bot},
            {component, <<"test.tg.localhost">>},
            {port, 8888}, 
            {password, <<"secret">>}, 
            {name, <<"botname">>}, 
            {host, <<"127.0.0.1">>}, 
            {nick, <<"tg_bot">>}, 
            {module, ebridgebot_tg},
            {token, <<"Telegram-token">>}, 
            {linked_rooms, [{-930545885, <<"my.room@conference.localhost">>}]}].
 [...]
$ ebridgebot_component:start(test_tg_bot, Args).
 {ok, <0.11591.0>}.
```

