-module(mochi_file).
-export([read_file/1, write_file/2, append_file/2, lines/1]).

%% read_file/1 — read entire file as a binary string.
read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%% write_file/2 — write Content (binary or string) to Path, overwriting.
write_file(Path, Content) ->
    ok = file:write_file(Path, Content),
    ok.

%% append_file/2 — append Content to Path.
append_file(Path, Content) ->
    ok = file:write_file(Path, Content, [append]),
    ok.

%% lines/1 — read file and return list of non-empty lines (binaries).
lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    Parts = binary:split(Bin, <<"\n">>, [global]),
    [L || L <- Parts, L =/= <<>>].
