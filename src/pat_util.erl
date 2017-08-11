-module(pat_util).

-export([parse_rfc822_addresses/1]).
-export([combine_rfc822_addresses/1]).

-define(is_whitespace(Ch), (Ch =< 32)).

parse_rfc822_addresses(String) ->
    Sc = scan_rfc822(String),
    rfc822_parse:parse(Sc).

scan_rfc822(String) when is_binary(String) ->
    scan_rfc822(binary_to_list(String));
scan_rfc822(String) when is_list(String) ->
    lists:reverse([{'$end', 0} | scan_rfc822(String, [])]).

scan_rfc822([], Acc) ->
    Acc;
scan_rfc822([Ch|R], Acc) when ?is_whitespace(Ch) ->
    scan_rfc822(R, Acc);
scan_rfc822([$"|R], Acc) ->
    {Token, Rest} = scan_rfc822_scan_endquote(R, [], false),
    scan_rfc822(Rest, [{string, 0, Token}|Acc]);
scan_rfc822([$,|Rest], Acc) ->
    scan_rfc822(Rest, [{',', 0}|Acc]);
scan_rfc822([$<|Rest], Acc) ->
    {Token, R} = scan_rfc822_scan_endpointybracket(Rest),
    scan_rfc822(R, [{'>', 0}, {string, 0, Token}, {'<', 0}|Acc]);
scan_rfc822(String, Acc) ->
    case re:run(String, "(.+?)([\s<>,].*)", [{capture, all_but_first, list}]) of
        {match, [Token, Rest]} ->
            scan_rfc822(Rest, [{string, 0, Token}|Acc]);
        nomatch ->
            [{string, 0, String}|Acc]
    end.

scan_rfc822_scan_endpointybracket(String) ->
    case re:run(String, "(.*?)>(.*)", [{capture, all_but_first, list}]) of
        {match, [Token, Rest]} ->
            {Token, Rest};
        nomatch ->
            {String, []}
    end.

scan_rfc822_scan_endquote([$\\|R], Acc, InEscape) ->
    scan_rfc822_scan_endquote(R, Acc, not(InEscape));
scan_rfc822_scan_endquote([$"|R], Acc, true) ->
    scan_rfc822_scan_endquote(R, [$"|Acc], false);
scan_rfc822_scan_endquote([$"|Rest], Acc, false) ->
    {lists:reverse(Acc), Rest};
scan_rfc822_scan_endquote([Ch|Rest], Acc, _) ->
    scan_rfc822_scan_endquote(Rest, [Ch|Acc], false).

combine_rfc822_addresses(Addresses) ->
    [_, _ | Acc] = combine_rfc822_addresses(Addresses, []),
    iolist_to_binary(lists:reverse(Acc)).

combine_rfc822_addresses([], Acc) ->
    Acc;
combine_rfc822_addresses([{undefined, Email} | Rest], Acc) ->
    combine_rfc822_addresses(Rest, [32, $,, Email | Acc]);
combine_rfc822_addresses([{Name, Email}|Rest], Acc) ->
    combine_rfc822_addresses(Rest, [32, $,, $>, Email, $<, 32, opt_quoted(Name)|Acc]).

opt_quoted(N)  ->
    case re:run(N, "\"") of
        nomatch -> N;
        {match, _} ->
            [$", re:replace(N, "\"", "\\\\\"", [global]), $"]
    end.
