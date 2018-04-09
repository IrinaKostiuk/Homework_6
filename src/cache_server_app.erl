-module(cache_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
            Dispatch = cowboy_router:compile([
                                              {'_', [{"/", cache_server_hendler, []},
                                                     {"/api/cache_server", cache_server_hendler, []}]}
                                             ]),

            {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                                        [{env, [{dispatch, Dispatch}]}]
                                       ),
            cache_server_sup:start_link().

stop(_State) ->
    ok.