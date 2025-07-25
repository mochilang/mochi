#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.25 on 2025-07-13T12:57:37Z
% q4.erl - generated from q4.mochi

main(_) ->
    Info_type = [#{id => 1, info => "rating"}, #{id => 2, info => "other"}],
    Keyword = [#{id => 1, keyword => "great sequel"}, #{id => 2, keyword => "prequel"}],
    Title = [#{id => 10, title => "Alpha Movie", production_year => 2006}, #{id => 20, title => "Beta Film", production_year => 2007}, #{id => 30, title => "Old Film", production_year => 2004}],
    Movie_keyword = [#{movie_id => 10, keyword_id => 1}, #{movie_id => 20, keyword_id => 1}, #{movie_id => 30, keyword_id => 1}],
    Movie_info_idx = [#{movie_id => 10, info_type_id => 1, info => "6.2"}, #{movie_id => 20, info_type_id => 1, info => "7.8"}, #{movie_id => 30, info_type_id => 1, info => "4.5"}],
    Rows = [#{rating => maps:get(info, Mi), title => maps:get(title, T)} || It <- Info_type, Mi <- Movie_info_idx, T <- Title, Mk <- Movie_keyword, K <- Keyword, (maps:get(id, It) == maps:get(info_type_id, Mi)), (maps:get(id, T) == maps:get(movie_id, Mi)), (maps:get(movie_id, Mk) == maps:get(id, T)), (maps:get(id, K) == maps:get(keyword_id, Mk)), (((((maps:get(info, It) == "rating") andalso (string:str(maps:get(keyword, K), "sequel") > 0)) andalso (maps:get(info, Mi) > "5.0")) andalso (maps:get(production_year, T) > 2005)) andalso (maps:get(movie_id, Mk) == maps:get(movie_id, Mi)))],
    Result = [#{rating => lists:min([maps:get(rating, R) || R <- Rows]), movie_title => lists:min([maps:get(title, R) || R <- Rows])}],
    mochi_json(Result),
    (case (Result == [#{rating => "6.2", movie_title => "Alpha Movie"}]) of true -> ok; _ -> erlang:error(test_failed) end).

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
