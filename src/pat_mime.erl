-module(pat_mime).

-export([text/0]).
-export([text/1]).
-export([text/2]).
-export([multipart/1]).
-export([base/1]).
-export([file/1]).
-export([add_header/2]).
-export([encode/1]).
-export([encode/2]).
-export([boundary/0]).
-export([boundary_start/1]).
-export([boundary_stop/1]).

-define(CHARSET, <<"utf-8">>).
-define(ENCODING, <<"base64">>).

text() ->
    text(<<"plain">>, ?CHARSET).

text(Type) when is_atom(Type) ->
    text(list_to_binary(atom_to_list(Type)), ?CHARSET);
text(Type) when is_binary(Type) ->
    text(Type, ?CHARSET).

text(Type, Charset) when is_binary(Type), is_binary(Charset) ->
   Headers = #{
       <<"Content-Type">> => iolist_to_binary([<<"text/">>, Type, <<"; charset=\"">>, Charset, "\""]),
       <<"Content-Transfer-Encoding">> => ?ENCODING
      },
    set_version(Headers).

multipart(Type) when is_atom(Type) ->
    multipart(list_to_binary(atom_to_list(Type)));
multipart(Type) ->
    Boundary = boundary(),
    Headers = #{
        <<"Content-Type">> => iolist_to_binary([<<"multipart/">>, Type, <<"; boundary=\"">>, Boundary, <<"\"">>])
       },
    Headers1 = set_version(Headers),
    #{ boundary => Boundary, headers => Headers1 }.

base(Type) ->
    Headers = #{
        <<"Content-Type">> => Type
       },
    set_version(Headers).

file(#{ filename := FileName, md5 := MD5, mimetype := Type }) ->
    Headers = #{
        <<"Content-Type">> => Type,
        <<"Content-Transfer-Encoding">> => ?ENCODING,
        <<"Content-Disposition">> => iolist_to_binary([<<"inline; filename=\"">>, FileName, <<"\"">>]),
        <<"Content-ID">> => iolist_to_binary([<<"<">>, MD5, <<">">>])
       },
    set_version(Headers).

add_header(Header, Headers) when is_map(Headers) ->
    maps:merge(Header, Headers).

encode(Headers) when is_map(Headers) ->
    encode(maps:to_list(Headers), <<"">>).

encode([], Acc) ->
    Acc;
encode([ { K, V } | [] ], Acc) ->
    Acc1 = iolist_to_binary([Acc, iolist_to_binary([ K, <<": ">>, V, <<"\n\n">> ])]),
    encode([], Acc1);
encode([ { K, V } | Tail ], Acc) ->
    Acc1 = iolist_to_binary([Acc, iolist_to_binary([ K, <<": ">>, V, <<"\n">> ])]),
    encode(Tail, Acc1).

boundary() ->
    Random = string:join([ebt_convert:to_l(N) || N <- ebt_random:numbers(19, 9)], ""),
    iolist_to_binary([<<"==========">>, Random, <<"==">>]).

boundary_start(Boundary) ->
    iolist_to_binary([<<"--">>, Boundary, <<"\n">>]).

boundary_stop(Boundary) ->
    iolist_to_binary([<<"--">>, Boundary, <<"--">>, <<"\n">>]).

set_version(Headers) ->
    Headers#{ <<"MIME-Version">> => <<"1.0">> }.
