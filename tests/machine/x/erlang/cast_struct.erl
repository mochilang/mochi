#!/usr/bin/env escript
% cast_struct.erl - generated from cast_struct.mochi

main(_) ->
    Todo = mochi_struct_cast(#{"title" => "hi"}),
    io:format("~p~n", [maps:get(title, Todo)]).

mochi_to_atom_key(K) when is_atom(K) -> K;
mochi_to_atom_key(K) when is_binary(K) -> list_to_atom(binary_to_list(K));
mochi_to_atom_key(K) when is_list(K) -> list_to_atom(K);
mochi_to_atom_key(K) -> K.

mochi_struct_cast(M) ->
    maps:from_list([{mochi_to_atom_key(K), V} || {K,V} <- maps:to_list(M)]).
