-module(cache_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [ ?CHILD(cache_server_srv, worker)],
	{ok, {{one_for_one, 1, 5}, Procs}}.
