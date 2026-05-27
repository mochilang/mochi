-module(mochi_str).
-export([print_float/1, concat/2, index/2, substring/3, reverse/1, convert/1, split/2, join/2,
         len/1, upper/1, lower/1, contains/2]).

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

%% concat/2 concatenates two binaries.
concat(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>.

%% index/2 — return the single-codepoint binary at position I (0-based).
index(S, I) ->
    Cps = string:to_graphemes(S),
    Cp = lists:nth(I + 1, Cps),
    unicode:characters_to_binary([Cp]).

%% substring/3 — return codepoints [Start, End).
substring(S, Start, End) ->
    Cps = string:to_graphemes(S),
    Slice = lists:sublist(Cps, Start + 1, End - Start),
    unicode:characters_to_binary(Slice).

%% reverse/1 — reverse string by codepoints.
reverse(S) ->
    Cps = string:to_graphemes(S),
    unicode:characters_to_binary(lists:reverse(Cps)).

%% convert/1 — convert integer or float to binary string.
convert(X) when is_integer(X) ->
    integer_to_binary(X);
convert(X) when is_float(X) ->
    float_to_binary(X);
convert(true) -> <<"true">>;
convert(false) -> <<"false">>.

%% split/2 — split S on Sep, return list of binaries (empty parts excluded).
split(S, Sep) ->
    Parts = binary:split(S, Sep, [global]),
    [P || P <- Parts, P =/= <<>>].

%% join/2 — join a list of binaries with Sep.
join([], _Sep) -> <<>>;
join([H | T], Sep) ->
    lists:foldl(fun(X, Acc) -> <<Acc/binary, Sep/binary, X/binary>> end, H, T).

%% len/1 — byte length of a binary string (matches C strlen semantics).
len(S) when is_binary(S) -> byte_size(S).

%% upper/1 — convert binary string to uppercase.
upper(S) when is_binary(S) ->
    unicode:characters_to_binary(string:uppercase(unicode:characters_to_list(S))).

%% lower/1 — convert binary string to lowercase.
lower(S) when is_binary(S) ->
    unicode:characters_to_binary(string:lowercase(unicode:characters_to_list(S))).

%% contains/2 — true if binary S contains substring Sub.
contains(S, Sub) when is_binary(S), is_binary(Sub) ->
    binary:match(S, Sub) =/= nomatch.
