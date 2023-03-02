-module(ebridgebot).
-compile(export_all).

-include("ebridgebot.hrl").

link_room(GroupId, MucJid, BotName) ->
	mnesia:dirty_write(#ebridgebot_muc{group_id = GroupId, muc_jid = MucJid, bot_name = BotName}).