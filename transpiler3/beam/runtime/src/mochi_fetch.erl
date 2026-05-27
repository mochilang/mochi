-module(mochi_fetch).
-export([get/1]).
-compile({no_auto_import,[get/1]}).

%% get/1 performs an HTTP GET on URL and returns the response body as a binary.
%% Uses OTP's built-in httpc client (part of the inets application).
%% TLS is handled automatically by OTP's ssl application (TLS 1.3 on OTP 27+).
%% Phase 14.0.
get(URL) when is_binary(URL) ->
    get(binary_to_list(URL));
get(URL) when is_list(URL) ->
    ok = ensure_inets(),
    case httpc:request(get, {URL, []}, [{ssl, [{verify, verify_peer},
                                               {cacerts, public_key:cacerts_get()}]},
                                        {timeout, 30000}], []) of
        {ok, {{_Version, 200, _Phrase}, _Headers, Body}} ->
            iolist_to_binary(Body);
        {ok, {{_Version, Status, Phrase}, _Headers, _Body}} ->
            error({http_error, Status, Phrase});
        {error, Reason} ->
            error({fetch_error, Reason})
    end.

ensure_inets() ->
    case application:start(inets) of
        ok             -> ok;
        {error, {already_started, inets}} -> ok;
        {error, Reason} -> error({inets_start_failed, Reason})
    end.
