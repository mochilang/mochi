-module(mochi_json).
-export([decode/1]).

%% decode/1 decodes a JSON object from a binary string and returns a
%% map<binary, binary> with all values coerced to their binary string
%% representations. Uses OTP 27 stdlib json:decode/1.
%%
%% Supported value coercions:
%%   null        -> <<"null">>
%%   true/false  -> <<"true">> / <<"false">>
%%   integer()   -> integer_to_binary/1
%%   float()     -> float_to_binary/2 with compact format
%%   binary()    -> kept as-is
%%   list/map    -> re-encoded to JSON via json:encode/1
%%
%% Phase 14.2.
decode(Bin) when is_binary(Bin) ->
    case json:decode(Bin) of
        Map when is_map(Map) ->
            maps:map(fun(_K, V) -> coerce(V) end, Map);
        _ ->
            #{}
    end.

coerce(null)           -> <<"null">>;
coerce(true)           -> <<"true">>;
coerce(false)          -> <<"false">>;
coerce(V) when is_integer(V) -> integer_to_binary(V);
coerce(V) when is_float(V)   -> float_to_binary(V, [{decimals, 10}, compact]);
coerce(V) when is_binary(V)  -> V;
coerce(V) ->
    try json:encode(V)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [V]))
    end.
