-module(ebridgebot).
-compile(export_all).

-include_lib("xmpp/include/xmpp.hrl").
-include("ebridgebot.hrl").

%% API
-spec gen_uuid() -> binary().
gen_uuid() ->
	list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

run_test() ->
	run_test(ebridgebot_tg_SUITE, []).
run_test(Suite) ->
	run_test(Suite, []).
run_test(Suite, Testcase) when is_atom(Testcase) ->
	run_test(Suite, [Testcase]);
run_test(Suite, Testcases) when is_atom(Suite) ->
	case application:get_env(ebridgebot, test_path, []) of
		[] -> ct:print("ERROR: test_path not found. Set test_path in ebridgebot section of sys.config", []),
			{error, invalid_test_path};
		TestPath ->
			ct:run_test(
				[{testcase, T} || T <- Testcases] ++
				[{suite, Suite},
					{dir, filename:join(TestPath, "test")},
					{include, ["_build/dev/lib/escalus", "include"]},
					{config, filename:join(TestPath, "test/test.config")}])
	end.

tag_decorator([], Data, Mod, Fun) ->
	fun() -> Mod:Fun(Data) end;
tag_decorator([El | TEls], [Pkt | _] = Data, Mod, Fun) ->
	case xmpp:get_subtag(Pkt, El) of
		false -> tag_decorator(TEls, Data, Mod, Fun);
		Tag -> fun() -> Mod:Fun(Tag, Data) end
	end.

wait_for_result(Fun, WaitedResult) ->
	wait_for_result(Fun, WaitedResult, 20, 100).
wait_for_result(Fun, _WaitedResultFun, 0, _) ->
	{error, wait_timout, Fun()};
wait_for_result(Fun, WaitedResultFun, Counter, Interval) when is_function(WaitedResultFun) ->
	Result = Fun(),
	case WaitedResultFun(Result) of
		true -> Result;
		_ ->
			timer:sleep(Interval),
			wait_for_result(Fun, WaitedResultFun, Counter-1, Interval)
	end;
wait_for_result(Fun, WaitedResult, Counter, Interval) ->
	case Fun() of
		WaitedResult -> WaitedResult;
		_ ->
			timer:sleep(Interval),
			wait_for_result(Fun, WaitedResult, Counter-1, Interval)
	end.

wait_for_list(Fun) ->
	wait_for_list(Fun, 0).
wait_for_list(Fun, Length) ->
	wait_for_list(Fun, Length, 20, 100).
wait_for_list(Fun, Length, Counter, Interval) when is_integer(Counter), is_integer(Interval), is_integer(Length)->
	PredFun =
		fun(Arg) when is_list(Arg), length(Arg) == Length -> true;
			(_Arg) -> false
		end,
	wait_for_result(Fun, PredFun, Counter, Interval).

%% component help API
bot_table(BotId) -> %% generate table name for bot
	list_to_atom(atom_to_list(BotId)++"_link").

-spec iq(list(muc_subscribe() | muc_unsubscribe()), jid(), jid()) -> iq().
iq(Els, From, To) when is_list(Els) ->
	#iq{type = set, from = From, to = To, sub_els = Els}.

-spec iq(subscribe | unsubscribe, jid(), jid(), binary()) -> iq().
iq(SubAction, From, To, Nick) ->
	iq(SubAction, From, To, Nick, <<>>).

-spec iq(subscribe | unsubscribe, jid(), jid(), binary(), binary()) -> iq().
iq(subscribe, From, To, Nick, Password) ->
	iq([#muc_subscribe{nick = Nick, password = Password, events = [?NS_MUCSUB_NODES_MESSAGES]}], From, To);
iq(unsubscribe, From, To, Nick, _Password) ->
	iq([#muc_unsubscribe{nick = Nick}], From, To).

-spec edit_msg(jid(), jid(), binary(), binary()) -> message().
edit_msg(From, To, Text, ReplaceId) ->
	OriginId = ebridgebot:gen_uuid(),
	#message{id = OriginId, type = groupchat, from = From, to = To, body = [#text{data = Text}],
		sub_els = [#origin_id{id = OriginId}, #replace{id = ReplaceId}]}.

-spec write_link(atom(), binary(), any()) -> ok.
write_link(BotId, OriginId, Uid) ->
	write_link(BotId, OriginId, Uid, []).

-spec write_link(atom(), binary(), any(), #mam_archived{} | binary() | false | []) -> ok.
write_link(BotId, OriginId, Uid, #mam_archived{id = MamId}) ->
	write_link(BotId, OriginId, Uid, MamId);
write_link(BotId, OriginId, Uid, false) ->
	write_link(BotId, OriginId, Uid);
write_link(BotId, OriginId, Uid, MamId) ->
	mnesia:dirty_write(setelement(1, #xmpp_link{origin_id = OriginId, mam_id = MamId, uid = Uid}, ebridgebot:bot_table(BotId))).

-spec upd_links(atom(), binary(), false | #mam_archived{}) -> ok.
upd_links(BotId, OriginId, false) ->
	index_read(BotId, OriginId, #xmpp_link.origin_id);
upd_links(BotId, OriginId, #mam_archived{id = MamId}) ->
	Table = ebridgebot:bot_table(BotId),
	Links = index_read(BotId, OriginId, #xmpp_link.origin_id),
	[mnesia:dirty_write(setelement(1, Link#xmpp_link{mam_id = MamId}, Table)) || Link <- Links].

-spec index_read(binary(), Key::term(), non_neg_integer()) -> list(#xmpp_link{}).
index_read(BotId, Key, Attr) ->
	[setelement(1, R, xmpp_link) || R <- mnesia:dirty_index_read(ebridgebot:bot_table(BotId), Key, Attr)].

to_rooms(CurChatId, Rooms, Fun) ->
	[Fun(ChatId, MucJid) || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
		CurChatId == ChatId andalso (E == in orelse S == subscribed)].