#!/usr/bin/env escript
%% https://www.spoj.com/problems/PRIME1/
%% Generates all prime numbers in given ranges using a segmented sieve.

main(_) ->
    {ok, [T]} = io:fread("", "~d"),
    Primes = sieve(32000),
    process_cases(T, Primes).

sieve(Limit) -> sieve(lists:seq(2, Limit), []).

sieve([], Acc) -> lists:reverse(Acc);
sieve([H|T], Acc) -> sieve([X || X <- T, X rem H =/= 0], [H|Acc]).

process_cases(0, _) -> ok;
process_cases(T, Primes) ->
    {ok, [M, N]} = io:fread("", "~d ~d"),
    start_segment(M, N, Primes),
    (case T of 1 -> ok; _ -> io:format("~n") end),
    process_cases(T-1, Primes).

start_segment(M, N, Primes) ->
    M1 = if M < 2 -> 2; true -> M end,
    Composites = mark_primes(Primes, M1, N, #{}),
    print_primes(M1, N, Composites).

mark_primes([], _, _, Map) -> Map;
mark_primes([P|_Ps], _M, N, Map) when P*P > N -> Map;
mark_primes([P|Ps], M, N, Map) ->
    Start0 = ((M + P - 1) div P) * P,
    Start = if Start0 < P*P -> P*P; true -> Start0 end,
    Map1 = mark(Start, N, P, Map),
    mark_primes(Ps, M, N, Map1).

mark(X, N, _, Map) when X > N -> Map;
mark(X, N, P, Map) -> mark(X+P, N, P, Map#{X => true}).

print_primes(M, N, _Map) when M > N -> ok;
print_primes(I, N, Map) ->
    case maps:is_key(I, Map) of
        false -> io:format("~B~n", [I]);
        true -> ok
    end,
    print_primes(I+1, N, Map).
