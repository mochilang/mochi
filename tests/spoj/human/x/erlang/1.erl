#!/usr/bin/env escript
%% https://www.spoj.com/problems/TEST/
%% Reads integers from standard input and prints each number until 42 is encountered.
%% After reading 42, the program terminates without printing it.

main(_) ->
    loop().

loop() ->
    case io:get_line("") of
        eof -> ok;
        Line ->
            case string:trim(Line) of
                "" -> loop();
                Str ->
                    {N, _} = string:to_integer(Str),
                    if N =:= 42 -> ok;
                       true ->
                           io:format("~B~n", [N]),
                           loop()
                    end
            end
    end.
