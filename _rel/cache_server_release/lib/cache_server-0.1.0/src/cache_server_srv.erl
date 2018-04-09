-module(cache_server_srv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define (TABLE, cach_table).
%---------------------------------------------------------------------
-export([start_link/0, insert/3, lookup/1, lookup_by_date/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Key, Value, Time) ->
    gen_server:call(?MODULE, {insert, {Key, Value, Time}}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

lookup_by_date(DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, {DateFrom, DateTo}}).
%% ====================================================================
init([]) -> 
    State = ets:new(cach_table, [public, named_table]),
    {ok, State}.
%-----------------------------------------------
handle_call({insert, {Key, Value, Time}}, _From, State) ->
    T = calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Time,
    Reply = case ets:insert(cach_table, {Key,Value, T}) of
                    true -> 
                        <<"ok">>;
                    _ -> <<"insert_error">>
                end,
    {reply, Reply, State};
%---------------------------------------------------------------------------
handle_call({lookup, Key}, _From, State) ->
    T_now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    Reply = case ets:lookup(cach_table, Key) of
                [{Key, _Value, Time}] when Time < T_now ->
                    <<"old data">>;
                [{Key, Value, Time}] when Time > T_now->
                     [Value, Time];
                 _ -> 
                    <<"underfined">>
            end,
    {reply, Reply, State};
%----------------------------------------------------------------------------------
handle_call({lookup_by_date, {DataFrom, DataTo}}, _From, State) ->
    DataF = calendar:datetime_to_gregorian_seconds(DataFrom),
    DataT = calendar:datetime_to_gregorian_seconds(DataTo),
    Key= ets:first(State),
    Reply = lookup_by_dat(Key, DataF, DataT, []),
    {reply, Reply, State};
%-----------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%------------------------------------------------
lookup_by_dat('$end_of_table', _, _, Acc) ->
    Acc;

lookup_by_dat(Key, DataFrom, DataTo, Acc) ->
    case ets:lookup(cach_table, Key) of
        [{Key, Value, Time}] when Time > DataFrom ->
            lookup_by_dat(ets:next(cach_table, Key), DataFrom, DataTo, [{Key, Value}|Acc]);
        _ -> 
            lookup_by_dat(ets:next(cach_table, Key), DataFrom, DataTo, Acc)
end.