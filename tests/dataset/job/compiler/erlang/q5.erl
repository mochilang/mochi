#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.25 on 2025-07-13T12:57:37Z
% q5.erl - generated from q5.mochi

main(_) ->
    Company_type = [#{ct_id => 1, kind => "production companies"}, #{ct_id => 2, kind => "other"}],
    Info_type = [#{it_id => 10, info => "languages"}],
    Title = [#{t_id => 100, title => "B Movie", production_year => 2010}, #{t_id => 200, title => "A Film", production_year => 2012}, #{t_id => 300, title => "Old Movie", production_year => 2000}],
    Movie_companies = [#{movie_id => 100, company_type_id => 1, note => "ACME (France) (theatrical)"}, #{movie_id => 200, company_type_id => 1, note => "ACME (France) (theatrical)"}, #{movie_id => 300, company_type_id => 1, note => "ACME (France) (theatrical)"}],
    Movie_info = [#{movie_id => 100, info => "German", info_type_id => 10}, #{movie_id => 200, info => "Swedish", info_type_id => 10}, #{movie_id => 300, info => "German", info_type_id => 10}],
    Candidate_titles = [maps:get(title, T) || Ct <- Company_type, Mc <- Movie_companies, Mi <- Movie_info, It <- Info_type, T <- Title, (maps:get(company_type_id, Mc) == maps:get(ct_id, Ct)), (maps:get(movie_id, Mi) == maps:get(movie_id, Mc)), (maps:get(it_id, It) == maps:get(info_type_id, Mi)), (maps:get(t_id, T) == maps:get(movie_id, Mc)), (((((maps:get(kind, Ct) == "production companies") andalso string:str(maps:get(note, Mc), "(theatrical)") > 0) andalso string:str(maps:get(note, Mc), "(France)") > 0) andalso (maps:get(production_year, T) > 2005)) andalso (lists:member(maps:get(info, Mi), ["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])))],
    Result = [#{typical_european_movie => lists:min(Candidate_titles)}],
    mochi_json(Result),
    (case (Result == [#{typical_european_movie => "A Film"}]) of true -> ok; _ -> erlang:error(test_failed) end).

mochi_escape_json([]) -> [];
mochi_escape_json([H|T]) ->
    E = case H of
        $\ -> "\\";
        $" -> "\"";
        _ -> [H]
    end,
    E ++ mochi_escape_json(T).

mochi_to_json(true) -> "true";
mochi_to_json(false) -> "false";
mochi_to_json(V) when is_integer(V); is_float(V) -> lists:flatten(io_lib:format("~p", [V]));
mochi_to_json(V) when is_binary(V) -> "\"" ++ mochi_escape_json(binary_to_list(V)) ++ "\"";
mochi_to_json(V) when is_list(V), (V =:= [] orelse is_integer(hd(V))) -> "\"" ++ mochi_escape_json(V) ++ "\"";
mochi_to_json(V) when is_list(V) -> "[" ++ lists:join(",", [mochi_to_json(X) || X <- V]) ++ "]";
mochi_to_json(V) when is_map(V) -> "{" ++ lists:join(",", ["\"" ++ atom_to_list(K) ++ "\":" ++ mochi_to_json(Val) || {K,Val} <- maps:to_list(V)]) ++ "}".

mochi_json(V) -> io:format("~s
", [mochi_to_json(V)]).
