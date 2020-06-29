emqx_backend_redis
===============

EMQ X Redis Data Save Plugin

Build Plugin
------------

```
make && make tests
```

Configure Plugin
----------------

File: etc/emqx_backend_redis.conf

```
##--------------------------------------------------------------------
## Redis Backend Plugin
##--------------------------------------------------------------------
## Redis Server cluster type
## single    Single redis server
## sentinel  Redis cluster through sentinel
## cluster   Redis through cluster
backend.redis.type = single

## Redis server address.
##
## Value: Port | IP:Port
##
## Single Redis Server: 127.0.0.1:6379, localhost:6379
## Redis Sentinel: 127.0.0.1:26379,127.0.0.2:26379,127.0.0.3:26379
## Redis Cluster: 127.0.0.1:6379,127.0.0.2:6379,127.0.0.3:6379
backend.redis.server = 127.0.0.1:6379

## Redis sentinel cluster name.
##
## Value: String
## backend.redis.sentinel = mymaster

## Redis pool size.
##
## Value: Number
backend.redis.pool = 8

## Redis database no.
##
## Value: Number
backend.redis.database = 0

## Redis password.
##
## Value: String
## auth.redis.password =

## Redis query timeout
##
## Value: Duration
## auth.redis.query_timeout = 5s

## 订阅的 Redis channel 名称
backend.redis.channel = mqtt_channel

backend.redis.client_connected_cmd = hmset mqtt:client:%c state 1 online_at %t
backend.redis.client_disconnected_cmd = hmset mqtt:client:%c state 0 offline_at %t
```

Function
-----------------

Now,plugin provide a template for redis-store,can store connected/disconnected info to
redis, if you have advanced function, can connect, I will update it.

Load Plugin
-----------

```
./bin/emqx_ctl plugins load emqx_backend_redis
```

Author
------

erl-go-java.

