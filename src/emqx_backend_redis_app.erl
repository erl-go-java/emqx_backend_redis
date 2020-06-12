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
    {ok, Sup} = emqx_auth_redis_sup:start_link(),
    load(),
    {ok, Sup}.

stop(_State) ->
    unload(),
    %% Ensure stop cluster pool if the server type is cluster
    eredis_cluster:stop_pool(?APP).

load() ->
    if_cmd_enabled(client_connected_cmd, fun load_client_connected/1).
%%    emqx:hook('client.disconnected', fun emqx_backend_redis:disconnected/3, []),
%%    emqx:hook('session.created',     fun emqx_backend_redis:created/3, []),
%%    emqx:hook('session.subscribed',  fun emqx_backend_redis:subscribed/3, []),
%%    emqx:hook('session.unsubscribed',fun emqx_backend_redis:unsubscribed/3, []),
%%    emqx:hook('message.publish',     fun emqx_backend_redis:publish/3, []),
%%    emqx:hook('message.acked',       fun emqx_backend_redis:acked/3, []).

load_client_connected(ClientConnectCmd) ->
    {ok, Timeout} = application:get_env(?APP, query_timeout),
    Config = #{
        client_connected_cmd => ClientConnectCmd,
        timeout => Timeout
    },
    emqx:hook('client.connected', fun emqx_backend_redis:on_client_connected/3, [Config]).

unload() ->
    emqx:unhook('client.connected',    fun emqx_backend_redis:on_client_connected/3),
    emqx:unhook('client.disconnected', fun emqx_backend_redis:disconnected/3),
    emqx:unhook('session.created',     fun emqx_backend_redis:created/3),
    emqx:unhook('session.subscribed',  fun emqx_backend_redis:subscribed/3),
    emqx:unhook('session.unsubscribed',fun emqx_backend_redis:unsubscribed/3),
    emqx:unhook('message.publish',     fun emqx_backend_redis:publish/3),
    emqx:unhook('message.acked',       fun emqx_backend_redis:acked/3).

if_cmd_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Cmd} -> Fun(Cmd);
        undefined -> ok
    end.