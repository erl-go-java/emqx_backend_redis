%%%-------------------------------------------------------------------
%%% @author jiefeng.chen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 6月 2020 14:56
%%%-------------------------------------------------------------------
-module(emqx_backend_redis_app).
-author("jiefeng.chen").

-behaviour(application).

-emqx_plugin(auth).

-include("emqx_backend_redis.hrl").

-export([ start/2
    , stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_backend_redis_sup:start_link(),
    load(),
    {ok, Sup}.

stop(_State) ->
    unload(),
    %% Ensure stop cluster pool if the server type is cluster
    eredis_cluster:stop_pool(?APP).

load() ->
    if_cmd_enabled(client_connected_cmd, fun load_client_connected/1),
    if_cmd_enabled(client_disconnected_cmd, fun load_client_disconnected/1),
    if_cmd_enabled(message_retain_cmd, fun load_message_retain_cmd/1).

load_client_connected(ClientConnectCmd) ->
    {ok, Timeout} = application:get_env(?APP, query_timeout),
    Config = #{
        client_connected_cmd => ClientConnectCmd,
        timeout => Timeout
    },
    emqx:hook('client.connected', fun emqx_backend_redis:on_client_connected/3, [Config]).

load_client_disconnected(ClientDisconnectedCmd) ->
    {ok, Timeout} = application:get_env(?APP, query_timeout),
    Config = #{
        client_disconnected_cmd => ClientDisconnectedCmd,
        timeout => Timeout
    },
    emqx:hook('client.disconnected', fun emqx_backend_redis:on_client_disconnected/4, [Config]).

load_message_retain_cmd(MessageRetainCmd) ->
    {ok, Timeout} = application:get_env(?APP, query_timeout),
    {ok, MaxRetainedMessages} = application:get_env(?APP, max_retained_messages),
    {ok, MaxPayloadSize} = application:get_env(?APP, max_payload_size),
    {ok, ExpiryInterval} = application:get_env(?APP, expiry_interval),
    Config = #{
        message_retain_cmd => MessageRetainCmd,
        timeout => Timeout,
        max_retained_messages => MaxRetainedMessages,
        max_payload_size => MaxPayloadSize,
        expiry_interval => ExpiryInterval
    },
    emqx:hook('message.publish', fun emqx_backend_redis:on_message_retain/2, [Config]).

unload() ->
    emqx:unhook('client.connected',    fun emqx_backend_redis:on_client_connected/3),
    emqx:unhook('client.disconnected', fun emqx_backend_redis:on_client_disconnected/4),
    emqx:unhook('message.publish', fun emqx_backend_redis:on_message_retain/2).

if_cmd_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Cmd} -> Fun(Cmd);
        undefined -> ok
    end.