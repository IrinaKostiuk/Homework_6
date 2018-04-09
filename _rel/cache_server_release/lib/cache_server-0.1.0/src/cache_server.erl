-module(cache_server).
-export([start/0]).

start() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    application:start(jsx),
    application:start(cache_server).