#!/usr/bin/env escript
-module(main).

-export([main/1]).

% Generated by Mochi transpiler v0.10.33 (88ad9915a) on 2025-07-21 19:10 +0700
main(_) ->
    X = 8,
    Msg = (case (X > 10) of
        true ->         "big"
;
        _ ->         (case (X > 5) of
            true ->             "medium"
;
            _ ->             "small"

        end)

    end),
    io:format("~s~n", [Msg])
.
