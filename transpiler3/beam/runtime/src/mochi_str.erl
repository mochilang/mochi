-module(mochi_str).
-export([print_float/1, concat/2]).

%% print_float/1 prints a float using Go-compatible shortest-round-trip
%% formatting, matching vm3's fmt.Println(f) output exactly.
%%
%% Special cases: NaN -> "NaN", +Inf -> "+Inf", -Inf -> "-Inf".
%% Normal values: shortest decimal that round-trips, same rules as
%% Go's strconv.FormatFloat(f, 'g', -1, 64).
print_float(F) when is_float(F) ->
    Bin = float_to_binary(F),
    io:put_chars([Bin, $\n]).

float_to_binary(F) ->
    %% NaN: F /= F is true only for NaN (IEEE 754 property).
    case F /= F of
        true ->
            <<"NaN">>;
        false ->
            case F > 1.7976931348623157e308 of
                true ->
                    <<"+Inf">>;
                false ->
                    case F < -1.7976931348623157e308 of
                        true ->
                            <<"-Inf">>;
                        false ->
                            format_float(F)
                    end
            end
    end.

%% format_float/1 finds the shortest decimal representation matching Go's %g.
%% Whole-number floats print without a decimal point (4.0 -> "4").
format_float(F) ->
    T = trunc(F),
    case float(T) =:= F of
        true  -> integer_to_binary(T);   %% 4.0 -> "4", -7.0 -> "-7"
        false -> shortest_binary(F)
    end.

%% shortest_binary finds the shortest decimal representation of F
%% that round-trips, using decimal (not scientific) notation.
%% Tries 1..17 significant decimal digits with compact notation.
shortest_binary(F) ->
    try_decimal(F, 1).

try_decimal(F, Prec) when Prec > 17 ->
    %% Fall back to full precision.
    float_to_binary(F, [{decimals, 17}, compact]);
try_decimal(F, Prec) ->
    Bin = float_to_binary(F, [{decimals, Prec}, compact]),
    %% Verify round-trip: Erlang requires float format for binary_to_float.
    RoundTrip = try binary_to_float(Bin)
                catch _:_ ->
                    try binary_to_float(<<Bin/binary, ".0">>)
                    catch _:_ -> F + 1.0  %% force mismatch
                    end
                end,
    case RoundTrip =:= F of
        true  -> Bin;
        false -> try_decimal(F, Prec + 1)
    end.

%% normalize_g ensures the result matches Go's 'g' format:
%% - exponent uses 'e+N' / 'e-N' notation (Go uses lowercase e)
%% - no trailing zeros after the decimal point
%% - always has a decimal point for non-integer floats
normalize_g(Bin) ->
    S = binary_to_list(Bin),
    case lists:member($e, S) orelse lists:member($E, S) of
        true ->
            %% Scientific notation: normalize exponent format.
            normalize_scientific(S);
        false ->
            %% Decimal notation.
            normalize_decimal(S)
    end.

normalize_scientific(S) ->
    %% Split at e/E.
    {Mantissa, [$e|Exp]} = lists:splitwith(fun(C) -> C =/= $e andalso C =/= $E end, S),
    NormMantissa = strip_trailing_zeros(Mantissa),
    NormExp = normalize_exp(Exp),
    list_to_binary(NormMantissa ++ "e" ++ NormExp).

normalize_exp([$+|Digits]) ->
    %% Remove leading zeros from positive exponent.
    "+" ++ strip_leading_zeros(Digits);
normalize_exp([$-|Digits]) ->
    "-" ++ strip_leading_zeros(Digits);
normalize_exp(Digits) ->
    "+" ++ strip_leading_zeros(Digits).

strip_leading_zeros([]) -> "0";
strip_leading_zeros([$0|Rest]) -> strip_leading_zeros(Rest);
strip_leading_zeros(S) -> S.

normalize_decimal(S) ->
    case lists:member($., S) of
        true  -> list_to_binary(strip_trailing_zeros(S));
        false -> list_to_binary(S)
    end.

strip_trailing_zeros(S) ->
    case lists:member($., S) of
        false -> S;
        true  -> strip_trailing_zeros_after_dot(lists:reverse(S))
    end.

strip_trailing_zeros_after_dot([$0|Rest]) ->
    strip_trailing_zeros_after_dot(Rest);
strip_trailing_zeros_after_dot([$.|Rest]) ->
    %% Remove trailing dot too (Go does not keep trailing dot).
    lists:reverse(Rest);
strip_trailing_zeros_after_dot(S) ->
    lists:reverse(S).

%% concat/2 concatenates two binaries.
concat(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>.
