#!/usr/bin/env escript
%% https://www.spoj.com/problems/ONP/
%% Convert infix algebraic expressions to Reverse Polish Notation.

main(_) ->
    {ok, [T]} = io:fread("", "~d"),
    process(T).

process(0) -> ok;
process(N) ->
    case io:get_line("") of
        eof -> ok;
        Line ->
            Expr = string:trim(Line),
            io:format("~s~n", [to_postfix(Expr)]),
            process(N-1)
    end.

%% Convert an infix expression (string) to postfix notation.
to_postfix(Str) ->
    lists:reverse(to_postfix_chars(Str, [], [])).

to_postfix_chars([], Out, []) -> Out;
to_postfix_chars([C|Rest], Out, Stack) when $a =< C, C =< $z ->
    to_postfix_chars(Rest, [C|Out], Stack);
to_postfix_chars([$\(|Rest], Out, Stack) ->
    to_postfix_chars(Rest, Out, [$\(|Stack]);
to_postfix_chars([$\)|Rest], Out, Stack) ->
    {Ops, Stack1} = pop_until_paren(Stack, []),
    to_postfix_chars(Rest, lists:reverse(Ops) ++ Out, Stack1);
to_postfix_chars([Op|Rest], Out, Stack) ->
    to_postfix_chars(Rest, Out, [Op|Stack]).

%% Pop operators from stack until an opening parenthesis is found.
pop_until_paren([$\(|Rest], Acc) -> {Acc, Rest};
pop_until_paren([Op|Rest], Acc) -> pop_until_paren(Rest, [Op|Acc]).
