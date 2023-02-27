%%%-------------------------------------------------------------------
%%% @author cryoflamer
%%% @copyright (C) 2023
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2023 7:57 PM
%%%-------------------------------------------------------------------
-author("cryoflamer").
%%
%%JID = exampleJid@nope.org
%%password = difficultPassword
%%muc_room = exampleMuc@muc.nope.org segunda@muc.sip.org
%%nick = jabbergram
%%token = jabbergramBotTokken
%%group = -10293943920 120301203

-record(ebrigebot_xmpp, {type = tg, jid, password, nick, token}).
-record(ebrigebot_muc, {muc_room, group}).
