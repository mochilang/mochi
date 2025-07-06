#!/usr/bin/env escript
%%! -noshell -smp enable

main([File]) ->
    case epp:parse_file(File, []) of
        {ok, Forms} -> output_funs(Forms);
        {error, Reason} -> io:format("{\"error\":\"~p\"}\n", [Reason]), halt(1)
    end,
    halt(0).

output_funs(Forms) ->
    Funs = [F || F <- Forms, element(1,F) == function],
    JsonFuns = [fun_to_json(F) || F <- Funs],
    io:format("{\"functions\":[~s]}\n", [string:join(JsonFuns, ",")]).

fun_to_json({function,Line,Name,A,[Clause|_]}) ->
    {clause,_,Params,_G,Body} = Clause,
    ParamNames = [param_name(P) || P <- Params],
    BodyLines = [string:trim(io_lib:format("~s", [erl_pp:expr(E)])) || E <- Body],
    lists:flatten([
        "{\"name\":", json_string(atom_to_list(Name)),
        ",\"params\":", json_array(ParamNames),
        ",\"body\":", json_array(BodyLines),
        ",\"line\":", integer_to_list(Line),
        ",\"arity\":", integer_to_list(A),
        "}"]).

param_name({var,_,Name}) -> atom_to_list(Name);
param_name({atom,_,Name}) -> atom_to_list(Name);
param_name(_) -> "_".

json_string(S) ->
    Esc = escape_string(S),
    lists:concat(["\"", Esc, "\""]).

escape_string([]) -> [];
escape_string([$\"|T]) -> [$\\,$\"|escape_string(T)];
escape_string([$\n|T]) -> [$\\,$n|escape_string(T)];
escape_string([$\r|T]) -> [$\\,$r|escape_string(T)];
escape_string([$\t|T]) -> [$\\,$t|escape_string(T)];
escape_string([H|T]) -> [H|escape_string(T)].

json_array(L) ->
    Items = [json_string(X) || X <- L],
    lists:concat(["[", string:join(Items, ","), "]"]).
