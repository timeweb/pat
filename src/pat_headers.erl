-module(pat_headers).

-export([encode/1]).
-export([encode/2]).

-define(MAXLINELEN, 76).
-define(MISCLEN, 7).
-define(CHARSET, <<"utf-8">>).

encode(Header) ->
    encode(Header, ?CHARSET).

encode(<<>>, _Charset) ->
    <<"">>;
encode(Header, Charset) when is_binary(Charset) ->
    encode(Header, binary_to_list(Charset));
encode({from, Value}, Charset) ->
    encode({<<"From">>, Value}, Charset);
encode({to, Value}, Charset) ->
    encode({<<"To">>, Value}, Charset);
encode({cc, Value}, Charset) ->
    encode({<<"Cc">>, Value}, Charset);
encode({bcc, Value}, Charset) ->
    encode({<<"Bcc">>, Value}, Charset);
encode({replayto, Value}, Charset) ->
    encode({<<"Replay-To">>, Value}, Charset);

encode({Header, Value}, Charset) when Header =:= <<"To">>; Header =:= <<"From">>;
                                      Header =:= <<"Cc">>; Header =:= <<"Bcc">>;
                                      Header =:= <<"Reply-To">> ->
    {ok, Addresses} = pat_util:parse_rfc822_addresses(Value),
    {Names, Emails} = lists:unzip(Addresses),
    Result = pat_util:combine_rfc822_addresses(lists:zip(names(Names, Charset), Emails)),
    [{Header, Result}];

encode(Header, Charset) ->
    MaxEncoded = ?MAXLINELEN - length(Charset) - ?MISCLEN,
    Header1 = binary:replace(Header, <<"(?<!\r)\n">>, <<"\r\n">>),
    Header2 = binary:replace(Header1, <<"\r(?!\n)">>, <<"\r\n">>),
    Header3 = base64:encode(Header2),
    encode(MaxEncoded, Header3, Charset).

encode(MaxEncoded, Header, Charset) ->
    encode(MaxEncoded, Header, Charset, []).

encode(MaxEncoded, Header, Charset, Acc) ->
    case Header of
        <<Head:MaxEncoded/binary, Tail/binary>> ->
            Header1 = struct(Charset, Head),
            encode(MaxEncoded, Tail, Charset, [Header1] ++ Acc);
        Head ->
            Header1 = struct(Charset, Head),
            list_to_binary(lists:reverse([Header1] ++ Acc))
    end.

struct(Charset, Header) ->
    iolist_to_binary(["=?", Charset, "?b?", Header, "?=", "\n"]).

names(Names, Charset) ->
    Names1 = [list_to_binary(N) || N <- Names ],
    [encode(N1, Charset) || N1 <- Names1].
