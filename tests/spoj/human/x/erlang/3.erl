#!/usr/bin/env escript
%% https://www.spoj.com/problems/SBSTR1
%% Read pairs of binary strings and output 1 if the second is a substring of the first, 0 otherwise.

main(_) ->
    loop().

loop() ->
    case io:fread("", "~s ~s") of
        eof -> ok;
        {ok, [A, B]} ->
            case string:str(A, B) of
                0 -> io:format("0~n");
                _ -> io:format("1~n")
            end,
            loop()
    end.
