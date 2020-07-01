%%%-------------------------------------------------------------------
%%% @author jiefeng.chen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 6月 2020 15:52
%%%-------------------------------------------------------------------
-module(emqx_backend_redis).
-author("jiefeng.chen").

-include("emqx_backend_redis.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

%% API
-export([
    on_client_connected/3,
    on_client_disconnected/4,
    on_message_retain/2
]).

on_client_connected(ClientInfo, _ConnInfo, #{
    client_connected_cmd := ClientConnectedCmd,
    timeout := Timeout
}) ->
    case emqx_backend_redis_cli:q(ClientConnectedCmd, ClientInfo, Timeout) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG(error, "[Redis] Command: ~p failed: ~p", [ClientConnectedCmd, Reason]),
            {error, not_found}
    end.

on_client_disconnected(ClientInfo, _Reason, _ConnInfo, #{
    client_disconnected_cmd := ClientDisConnectedCmd,
    timeout := Timeout
}) ->
    case emqx_backend_redis_cli:q(ClientDisConnectedCmd, ClientInfo, Timeout) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG(error, "[Redis] Command: ~p failed: ~p", [ClientDisConnectedCmd, Reason]),
            {error, not_found}
    end.

on_message_retain(Msg = #message{flags = #{retain := true}}, Env) ->
    Msg1 = emqx_message:set_header(retained, true, Msg),
    store_retained(Msg, Env),
    {ok, Msg};

on_message_retain(Msg, _Env) ->
    {ok, Msg}.

store_retained(Msg = #message{topic = Topic, payload = Payload, timestamp = Ts}, Env) ->
    case {is_table_full(Env), is_too_big(size(Payload), Env)} of
        {false, false} ->
            BackendRedisMessage = message_to_redis_message(Msg, true),
            MessageRetainTopic  = maps:get(message_retain_cmd, Env) ++ binary_to_list(Msg#message.topic),
            MsgList = lists:flatten(
                lists:zipwith(
                    fun(X,Y) ->
                        [X,Y]
                    end,
                    record_info(fields, backend_redis_message),
                    tl(tuple_to_list(BackendRedisMessage)
                    )
                )
            ),
            Cmd = [?HMSET, MessageRetainTopic | MsgList],
            Timeout = maps:get(timeout, Env),
            case emqx_backend_redis_cli:q(Cmd, Timeout) of
                {ok, _} ->
                    ExpireTime = case Msg of
                        #message{topic = <<"$SYS/", _/binary>>} -> 0;
                        #message{headers = #{'Message-Expiry-Interval' := Interval}, timestamp = Ts} when Interval =/= 0 ->
                            Interval;
                        #message{timestamp = Ts} ->
                            maps:get(expiry_interval, Env, 0)
                    end,
                    if
                        ExpireTime > 0 ->
                            emqx_backend_redis_cli:expire(MessageRetainTopic, ExpireTime);
                        true ->
                            noop
                    end,
                    ok = emqx_metrics:set('messages.retained', retained_count());
                {error, Reason} ->
                    ?LOG(error, "[Redis] Command: ~p failed: ~p", [Cmd, Reason])
            end;
        {true, _} ->
            ?LOG(error, "[Retainer] Cannot retain message(topic=~s) for table is full!", [Topic]);
        {_, true} ->
            ?LOG(error, "[Retainer] Cannot retain message(topic=~s, payload_size=~p) "
            "for payload is too big!", [Topic, iolist_size(Payload)])
    end.

is_table_full(Env) ->
    Limit = maps:get(max_retained_messages, Env, 0),
    Limit > 0 andalso (retained_count() > Limit).

is_too_big(Size, Env) ->
    Limit = maps:get(max_payload_size, Env, 0),
    Limit > 0 andalso (Size > Limit).

-spec(retained_count() -> non_neg_integer()).
retained_count() ->
    retained_count(0, []).

retained_count(Cursor, List) ->
    case scan(Cursor, ?RETAIN_KEY, ?SCAN_COUNT) of
        [] ->
            length(lists:usort(List));
        ReturnList ->
            NextCursor = hd(NextCursor),
            case NextCursor of
                <<"0">> ->
                    length(lists:usort([tl(ReturnList) | List]));
                _ ->
                    retained_count(binary_to_list(NextCursor), [tl(ReturnList) | List])
            end
    end.

message_to_redis_message(Msg, IsRetain) ->
    #backend_redis_message{
        id = Msg#message.id,
        from = Msg#message.from,
        qos = Msg#message.qos,
        topic = Msg#message.topic,
        retain = IsRetain,
        payload = Msg#message.payload,
        ts = Msg#message.timestamp
    }.