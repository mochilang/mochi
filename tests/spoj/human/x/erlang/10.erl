#!/usr/bin/env escript
%% https://www.spoj.com/problems/CMEXPR/
%% Remove redundant parentheses from arithmetic expressions.

main(_) ->
    Line = io:get_line(""),
    {T, _} = string:to_integer(string:trim(Line)),
    process(T).

process(0) -> ok;
process(N) ->
    case io:get_line("") of
        eof -> ok;
        Line ->
            Expr = string:trim(Line),
            Tree = parse(Expr),
            io:format("~s", [format(Tree, 0, true)]),
            if N > 1 -> io:format("~n", []); true -> ok end,
            process(N - 1)
    end.

parse(S) ->
    {Node, _} = parse_expr(S),
    Node.

parse_expr(S) ->
    {N1, Rest1} = parse_term(S),
    parse_expr_rest(N1, Rest1).

parse_expr_rest(Acc, [$+|Rest]) ->
    {N2, Rest2} = parse_term(Rest),
    parse_expr_rest({op, $+, Acc, N2}, Rest2);
parse_expr_rest(Acc, [$-|Rest]) ->
    {N2, Rest2} = parse_term(Rest),
    parse_expr_rest({op, $-, Acc, N2}, Rest2);
parse_expr_rest(Acc, Rest) ->
    {Acc, Rest}.

parse_term(S) ->
    {N1, Rest1} = parse_factor(S),
    parse_term_rest(N1, Rest1).

parse_term_rest(Acc, [$*|Rest]) ->
    {N2, Rest2} = parse_factor(Rest),
    parse_term_rest({op, $*, Acc, N2}, Rest2);
parse_term_rest(Acc, [$/|Rest]) ->
    {N2, Rest2} = parse_factor(Rest),
    parse_term_rest({op, $/, Acc, N2}, Rest2);
parse_term_rest(Acc, Rest) ->
    {Acc, Rest}.

parse_factor([$(|Rest]) ->
    {N, [$)|Rest2]} = parse_expr(Rest),
    {N, Rest2};
parse_factor([C|Rest]) ->
    {{val, C}, Rest}.

precedence($+) -> 1;
precedence($-) -> 1;
precedence($*) -> 2;
precedence($/) -> 2;
precedence(_) -> 0.

format({val, C}, _, _) ->
    [C];
format({op, Op, L, R}, Parent, IsLeft) ->
    PrecP = precedence(Parent),
    PrecC = precedence(Op),
    Need = (PrecC < PrecP) orelse
           (PrecC == PrecP andalso (Parent == $- orelse Parent == $/) andalso not IsLeft),
    Left = format(L, Op, true),
    Right = format(R, Op, false),
    Expr = Left ++ [Op] ++ Right,
    case Need of
        true -> [$(] ++ Expr ++ [$)];
        false -> Expr
    end.
