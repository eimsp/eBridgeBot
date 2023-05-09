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

-spec wait_for_result(function(), function() | term()) -> any().
wait_for_result(Fun, WaitedResult) ->
	wait_for_result(Fun, WaitedResult, 20, 100).

-spec wait_for_result(function(), function() | term(), integer(), integer()) -> any().
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

-spec wait_for_list(function()) -> any().
wait_for_list(Fun) ->
	wait_for_list(Fun, 0).

-spec wait_for_list(function(), integer()) -> any().
wait_for_list(Fun, Length) ->
	wait_for_list(Fun, Length, 20, 100).

-spec wait_for_list(function(), integer(), integer(), neg_integer()) -> any().
wait_for_list(Fun, Length, Counter, Interval) when is_integer(Counter), is_integer(Interval), is_integer(Length)->
	PredFun =
		fun(Arg) when is_list(Arg), length(Arg) == Length -> true;
			(_Arg) -> false
		end,
	wait_for_result(Fun, PredFun, Counter, Interval).

%% component help API

-spec tag_decorator(list(xmpp_element()), list(xmpp_element() | list(any())), atom(), atom()) -> function().
tag_decorator([], Data, Mod, Fun) ->
	fun() -> Mod:Fun(Data) end;
tag_decorator([El | TEls], [Pkt | _] = Data, Mod, Fun) ->
	case xmpp:get_subtag(Pkt, El) of
		false -> tag_decorator(TEls, Data, Mod, Fun);
		Tag -> fun() -> Mod:Fun(Tag, Data) end
	end.

-spec bot_table(atom()) -> atom().
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

-spec password(#{type => atom(), password => binary() | undefined}) -> binary() | undefined.
password(#{type := Type, password := <<_/integer, _/binary>> = Pwd}) when Type == available; Type == subscribe -> Pwd;
password(#{type := Type}) when Type == unsubscribe; Type == subscribe -> <<>>;
password(_) -> undefined.

-spec edit_msg(jid(), jid(), binary(), binary()) -> message().
edit_msg(From, To, Text, ReplaceId) ->
	edit_msg(From, To, Text, ReplaceId, []).
-spec edit_msg(jid(), jid(), binary(), binary(), list(term())) -> message().
edit_msg(From, To, Text, ReplaceId, SubEls) ->
	OriginId = ebridgebot:gen_uuid(),
	#message{id = OriginId, type = groupchat, from = From, to = To, body = [#text{data = Text}],
		sub_els = [#origin_id{id = OriginId}, #replace{id = ReplaceId} | SubEls]}.

-spec write_link(atom(), binary(), any()) -> ok.
write_link(BotId, OriginId, Uid) ->
	write_link(BotId, OriginId, Uid, []).

-spec write_link(atom(), binary() | #origin_id{} | false | [], any(), #mam_archived{} | binary() | false | []) -> ok.
write_link(BotId, #origin_id{id = OriginId}, Uid, MamId) ->
	write_link(BotId, OriginId, Uid, MamId);
write_link(BotId, OriginId, Uid, #mam_archived{id = MamId}) ->
	write_link(BotId, OriginId, Uid, MamId);
write_link(BotId, OriginId, Uid, false) ->
	write_link(BotId, OriginId, Uid, []);
write_link(BotId, false, Uid, MamId) ->
	write_link(BotId, [], Uid, MamId);
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

-spec to_rooms(integer() | binary(), list(#muc_state{}), function()) -> list(function()).
to_rooms(CurChatId, Rooms, Fun) ->
	[Fun(MucJid) || #muc_state{group_id = ChatId, muc_jid = MucJid, state = {E, S}} <- Rooms,
		CurChatId == ChatId andalso (E == in orelse S == subscribed)].

-spec merge_entities(list(entity())) -> list(entity()).
merge_entities(Entities) ->
	lists:flatten(tuple_to_list(
		lists:foldl(
			fun(#entity{} = E, {[], Acc}) ->
					{E, Acc};
				(#entity{offset = Offset, length = Length, type = Type},
				 {#entity{offset = LastOffset, length = LastLength, type = Type} = E, Acc})
					when LastOffset + LastLength == Offset ->
					{E#entity{length = Offset + Length - LastOffset}, Acc};
				(#entity{} = E, {#entity{} = E2, Acc}) ->
					{E, Acc ++ [E2]}
			end, {[], []}, Entities))).

-spec pkt_fun() -> function().
pkt_fun() ->
	fun(#message{} = Pkt, To) ->
		Id = gen_uuid(),
		xmpp:set_subtag(Pkt#message{id = Id, to = jid:decode(To)}, #origin_id{id = Id})
	end.

-spec pkt_fun(type | from | tag | text, function(), binary() | xmpp_element()) -> function().
pkt_fun(type, PktFun, Type) ->
	fun(#message{} = Pkt, To) -> (PktFun(Pkt, To))#message{type = Type} end;
pkt_fun(from, PktFun, From) ->
	fun(#message{} = Pkt, To) -> (PktFun(Pkt, To))#message{from = jid:decode(From)} end;
pkt_fun(tag, PktFun, Tag) ->
	fun(#message{} = Pkt, To) -> xmpp:set_subtag(PktFun(Pkt, To), Tag) end;
pkt_fun(text, PktFun, Text) ->
	fun(#message{} = Pkt, To) ->
		case PktFun(Pkt, To) of
			#message{body = [#text{data = OrigText}]} = Pkt2 ->
				Pkt2#message{body = [#text{data = <<OrigText/binary, Text/binary>>}]};
			#message{body = []} = Pkt2 ->
				Pkt2#message{body = [#text{data = Text}]}
		end
	end.

-spec fold_pkt_fun(list({type | from | tag | text, binary() | xmpp_element()}), function()) -> function().
fold_pkt_fun([], PktFun) -> PktFun;
fold_pkt_fun([{K, V} | T], PktFun) ->
	fold_pkt_fun(T, pkt_fun(K, PktFun, V)).

-spec send_to(pid() | term(), function(), binary(), atom(), term()) -> ok.
send_to(Client, PktFun, To, BotId, Uid) ->
	Module = case is_pid(Client) of true -> escalus_component; _ -> escalus end,
	Module:send(Client, xmpp:encode(#message{id = Id} = PktFun(#message{}, To))),
	ebridgebot:write_link(BotId, Id, Uid).