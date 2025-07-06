#!/usr/bin/env escript
%%! -noshell -smp enable

main([File]) ->
    case epp:parse_file(File, []) of
        {ok, Forms} -> output_ast(Forms);
        {error, Reason} -> io:format("{\"error\":\"~p\"}\n", [Reason]), halt(1)
    end,
    halt(0).

output_ast(Forms) ->
    Funs = [F || F <- Forms, element(1,F) == function],
    Records = [R || R <- Forms, element(1,R) == attribute, element(3,R) == record],
    Exports = case [E || E <- Forms, element(1,E) == attribute, element(3,E) == export] of
        [E] -> element(4, E);
        _ -> []
    end,
    Module = case [M || {attribute,_,module,M} <- Forms] of
        [N] -> atom_to_list(N);
        _ -> ""
    end,
    JsonFuns = [fun_to_json(F, Exports) || F <- Funs],
    JsonRecs = [record_to_json(R) || R <- Records],
    io:format("{\"module\":~s,\"functions\":[~s],\"records\":[~s]}\n",
              [json_string(Module), string:join(JsonFuns, ","), string:join(JsonRecs, ",")]).
fun_to_json({function,Line,Name,A,[Clause|_]}, Exports) ->
    {clause,_,Params,_G,Body} = Clause,
    ParamNames = [param_name(P) || P <- Params],
    BodyLines = [string:trim(io_lib:format("~s", [erl_pp:expr(E)])) || E <- Body],
    Exported = lists:any(fun({N,Ar}) -> N == Name andalso Ar == A end, Exports),
    lists:flatten([
        "{\"name\":", json_string(atom_to_list(Name)),
        ",\"params\":", json_array(ParamNames),
        ",\"body\":", json_array(BodyLines),
        ",\"line\":", integer_to_list(Line),
        ",\"arity\":", integer_to_list(A),
        ",\"exported\":", if Exported -> "true"; true -> "false" end,
        "}"]).

record_to_json({attribute,Line,record,{Name,Fields}}) ->
    FieldNames = [atom_to_list(F) || {record_field,_,{atom,_,F}} <- Fields],
    lists:flatten([
        "{\"name\":", json_string(atom_to_list(Name)),
        ",\"fields\":", json_array(FieldNames),
        ",\"line\":", integer_to_list(Line),
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
