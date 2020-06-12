%%%-------------------------------------------------------------------
%%% @author jiefeng.chen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(emqx_backend_redis_sup).

-behaviour(supervisor).

-include("emqx_backend_redis.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Server} = application:get_env(?APP, server),
    {ok, {{one_for_one, 10, 100}, pool_spec(Server)}}.

pool_spec(Server) ->
    case proplists:get_value(type, Server) of
        cluster ->
            eredis_cluster:start_pool(?APP, Server),
            [];
        _ ->
            [ecpool:pool_spec(?APP, ?APP, emqx_backend_redis_cli, Server)]
    end.
