-module (cache_server_hendler).

-export([init/3, handle/2, terminate/3]).

%--------------------------------------------------
init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, ReqRest} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(ReqRest),
	Request = parse_req(Method, HasBody, ReqRest),
	{Request, State}.

terminate(_Reason, _Req, _State) ->
	ok.
%--------------------------------------------------
parse_req(<<"POST">>, true, Req) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
        [{BinJson, true}] = KeyValues,
        Json = jsx:decode(BinJson),
        Action = proplists:get_value(<<"action">>, Json),

        RespBody = jsx:encode([<<"result">>, parse_command(Action, Json)]),
       
        cowboy_req:reply(200, [
                               {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                              ], RespBody, Req2);

parse_req(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Json, Where is your body?">>, Req);

parse_req(<<"GET">>, _, Req) ->
    cowboy_req:reply(200, [], <<"You can POST !!!">>, Req);

parse_req(_, _, Req) ->
    cowboy_req:reply(405, Req).
%------------------------------------------------------
parse_command(<<"insert">>, Json) ->
    Key = proplists:get_value(<<"key">>, Json),
    Value = proplists:get_value(<<"value">>, Json),
    Time = proplists:get_value(<<"time">>, Json),
    cache_server_srv:insert(Key, Value, Time);

parse_command(<<"lookup">>, Json) ->
    Key = proplists:get_value(<<"key">>, Json),
    cache_server_srv:lookup(Key);

parse_command(<<"lookup_by_date">>, Json) ->
    DataFrom = proplists:get_value(<<"date_from">>, Json),
    DataTo = proplists:get_value(<<"date_to">>, Json),
    DataF = binary_to_datetime(DataFrom),
    DataT = binary_to_datetime(DataTo),
    cache_server_srv:lookup_by_date(DataF, DataT);

parse_command(_, _) ->
    <<"OOPS">>.
%-------------------------------------------------------------------------
binary_to_datetime(BinDate) ->
    [Date, Time] = split(BinDate, <<" ">>),
    [Year, Month, Day] = split(Date, <<"/">>),
    [Hour, Min, Sec] = split(Time, <<":">>),
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
    {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}}.

split(Bin, Split) when is_binary(Split) ->
    split(Bin, byte_size(Split), Split, <<>>, []).

split(Bin, SplitSize, Split, BinAcc, Acc) ->
    case Bin of
        <<Split:SplitSize/binary, Rest/binary>> ->
            split(Rest, SplitSize, Split, <<>>, [BinAcc | Acc]);
        <<H:1/binary, Rest/binary>> ->
            split(Rest, SplitSize, Split, <<BinAcc/binary, H:1/binary>>, Acc);
        <<>> ->
            lists:reverse([BinAcc | Acc])
    end.