#!/usr/bin/env escript
%% https://www.spoj.com/problems/ARITH
%% Prints arithmetic expressions in the same format used for manual calculations.

main(_) ->
    {ok, [T]} = io:fread("", "~d"),
    loop_cases(T).

loop_cases(0) -> ok;
loop_cases(N) ->
    {ok, [Expr]} = io:fread("", "~s"),
    print_case(Expr),
    case N of
        1 -> ok;
        _ -> io:format("~n"), loop_cases(N - 1)
    end.

find_op(Str) ->
    case string:chr(Str, $+) of
        0 -> case string:chr(Str, $-) of
                 0 -> string:chr(Str, $*);
                 Pos -> Pos
             end;
        Pos -> Pos
    end.

print_case(Expr) ->
    OpPos = find_op(Expr),
    Op = lists:nth(OpPos, Expr),
    AStr = string:sub_string(Expr, 1, OpPos - 1),
    BStr = string:sub_string(Expr, OpPos + 1),
    A = list_to_integer(AStr),
    B = list_to_integer(BStr),
    ResStr = integer_to_list(case Op of
        $+ -> A + B;
        $- -> A - B;
        $* -> A * B
    end),
    OpLine = [Op] ++ BStr,
    case Op of
        $+ -> print_add_sub(AStr, OpLine, ResStr);
        $- -> print_add_sub(AStr, OpLine, ResStr);
        $* -> print_mul(AStr, BStr, OpLine, ResStr, A)
    end.

print_add_sub(AStr, OpLine, ResStr) ->
    Width = max_int([length(AStr), length(OpLine), length(ResStr)]),
    LineLen = max_int([length(OpLine), length(ResStr)]),
    print_line(Width, AStr),
    print_line(Width, OpLine),
    print_dash(Width, LineLen),
    print_line(Width, ResStr).

print_mul(AStr, BStr, OpLine, ResStr, A) ->
    Parts = build_parts(A, BStr),
    {FirstP, _} = hd(Parts),
    MaxPart = lists:max([length(P) + S || {P, S} <- Parts]),
    Width = max_int([length(AStr), length(OpLine), length(ResStr), MaxPart]),
    LineLen = max_int([length(OpLine), length(FirstP)]),
    print_line(Width, AStr),
    print_line(Width, OpLine),
    print_dash(Width, LineLen),
    case Parts of
        [_Single] -> print_line(Width, ResStr);
        _ ->
            lists:foreach(fun({P, S}) -> print_part(Width, P, S) end, Parts),
            io:format("~s~n", [string:copies("-", Width)]),
            print_line(Width, ResStr)
    end.

build_parts(A, BStr) ->
    build_parts(A, lists:reverse(BStr), [], 0).

build_parts(_, [], Acc, _) -> lists:reverse(Acc);
build_parts(A, [D|Rest], Acc, Shift) ->
    Digit = D - $0,
    PStr = if Digit =:= 0 -> "0"; true -> integer_to_list(A * Digit) end,
    build_parts(A, Rest, [{PStr, Shift}|Acc], Shift + 1).

print_part(Width, P, Shift) ->
    Spaces = Width - Shift - length(P),
    io:format("~s~n", [string:copies(" ", Spaces) ++ P]).

print_line(Width, Str) ->
    io:format("~*s~n", [Width, Str]).

print_dash(Width, Len) ->
    io:format("~s~n", [string:copies(" ", Width - Len) ++ string:copies("-", Len)]).

max_int(List) ->
    lists:max(List).
