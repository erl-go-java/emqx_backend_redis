%%%-------------------------------------------------------------------
%%% @author jiefeng.chen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 6月 2020 14:58
%%%-------------------------------------------------------------------
-author("jiefeng.chen").

-define(APP, emqx_backend_redis).

-define(HMSET, "hmset").

-record(backend_redis_message, {
  %% Global unique message ID
  id :: binary(),
  %% Message QoS
  qos = 0,
  %% Message from
  from :: atom() | binary(),
  %% Message flags
  retain :: #{atom() => boolean()},
  %% Topic that the message is published to
  topic :: binary(),
  %% Message Payload
  payload :: binary(),
  %% Timestamp (Unit: millisecond)
  ts :: integer()
}).