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
    on_client_disconnected/4
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

on_message_retain(Msg = #message{flags = #{retain := true}}, #{
    message_retain_cmd := MessageRetainCmd,
    timeout := Timeout
}) ->
    redisFieldValue = lists:concat(["id ", Msg#message.id, " from ", Msg#message.from, " qos ", Msg#message.qos,
        " topic ", Msg#message.topic, " retain true payload ", Msg#message.payload, " ts ", Msg#message.timestamp]),
    case emqx_backend_redis_cli:q(MessageRetainCmd, #{topic => Msg#message.topic, string => redisFieldValue}, Timeout) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG(error, "[Redis] Command: ~p failed: ~p", [MessageRetainCmd, Reason]),
            {error, not_found}
    end,
    {ok, Msg};

on_message_retain(Msg, _Env) ->
    {ok, Msg}.

