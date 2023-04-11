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
communication with Telegram is implemented through its [bot API](https://core.telegram.org/bots/api).
The Jabber xmpp server and client must support the following protocols for message manipulation:
* XEP-0425 for [message moderation](https://xmpp.org/extensions/xep-0425.html).
* XEP-0424 to [delete messages](https://xmpp.org/extensions/xep-0424.html).
* XEP-0308 to [message replace](https://xmpp.org/extensions/xep-0308.html).

The application can be configured by modifying the `etc/sys.config` file. The following configuration parameters are available:

```erlang
% First of all, you need to get bot's credentials.
% Consult telegram docs https://core.telegram.org/bots#6-botfather

[{ebridgebot,
    [{bots, %% list of bots
        [#{bot_id => tg_bot,  %% unique id of the bot
        component => <<"tg.localhost">>, %% XMPP component host name
        bot_name => <<"ebridge_bot">>, %% The telegram bot name
        host => <<"127.0.0.1">>, %% XMPP server address
        upload_host => <<"upload.localhost">>,
        upload_endpoint => undefined, %% url upload endpoint must be without port and not localhost
        nick => <<"tg_bot">>, %% XMPP nick
        password => <<"secret">>, %% XMPP component password
        module => ebridgebot_tg, %% implement Telegram callbacks or other third party messenger
        port => 8888, %% XMPP service port
        token => <<"Telegram-token">>, %% telegram bot API token
        ignore_commands => true, %% ignore commands from chats
        format => #{usernick => italic, system => pre}, %% format message output 
        rooms => [{-1111111111111, #{jid => <<"my.room@conference.localhost">>}}]} %% optional #{password => undefined} by default for xmpp muc
    ]}]}].
```

## Examples

### Shell
``` erlang
$ rebar3 shell
```
####start bot component manually

```erlang
$ Args = #{bot_id => tg_bot,
            component => <<"tg.localhost">>,
            bot_name => <<"ebridge_bot">>,
            host => <<"127.0.0.1">>,
            upload_host => <<"upload.localhost">>,
            upload_endpoint => undefined, %% url upload endpoint must be without port and not localhost
            component => <<"tg.localhost">>,
            nick => <<"tg_bot">>,
            password => <<"secret">>,
            module => ebridgebot_tg,
            port => 8888,
            token => <<"6066841531:AAEK0aUdaP6eoJWcS0020VOyYQpNhhMpBPr">>,
            ignore_commands => true,
            format => #{usernick => italic, system => pre},
            rooms => [{-1001942208833, #{jid => <<"my.room@conference.localhost">>}}.
 [...]
$ ebridgebot_component:start(Args).
 {ok, <0.11591.0>}.
```

