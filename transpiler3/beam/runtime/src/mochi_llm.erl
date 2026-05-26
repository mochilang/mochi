%% mochi_llm: LLM generation for the Mochi BEAM runtime.
%%
%% MEP-46 Phase 13.0: cassette-backed LLM generation.
%%
%% Cassette playback mode (MOCHI_LLM_CASSETTE_DIR env set):
%%   Looks up a pre-recorded response file named by the DJB2 hash of the
%%   concatenated key "provider\0model\0prompt" in the cassette directory.
%%   File name format: "<hash_decimal>.txt". Returns the file contents as
%%   a binary, stripping any trailing newline.
%%
%% Live mode (MOCHI_LLM_CASSETTE_DIR not set):
%%   Not yet implemented in this phase; returns empty binary with a warning
%%   to stderr. Set MOCHI_LLM_CASSETTE_DIR for test mode.
-module(mochi_llm).
-export([generate/3]).

%% generate(Provider, Model, Prompt) -> binary()
%% Provider, Model, Prompt are UTF-8 binaries.
generate(Provider, Model, Prompt) ->
    case os:getenv("MOCHI_LLM_CASSETTE_DIR") of
        false ->
            io:format(standard_error, "mochi_llm: MOCHI_LLM_CASSETTE_DIR not set; returning empty string~n", []),
            <<>>;
        Dir ->
            cassette_lookup(Dir, Provider, Model, Prompt)
    end.

%% cassette_lookup: look up DJB2-keyed cassette file.
cassette_lookup(Dir, Provider, Model, Prompt) ->
    Hash = djb2_key(Provider, Model, Prompt),
    HashStr = integer_to_list(Hash),
    Path = filename:join(Dir, HashStr ++ ".txt"),
    case file:read_file(Path) of
        {ok, Data} ->
            %% Strip trailing newline to match C runtime behaviour.
            strip_trailing_newline(Data);
        {error, Reason} ->
            io:format(standard_error, "mochi_llm: cassette ~s not found: ~p~n", [Path, Reason]),
            <<>>
    end.

%% djb2_key: DJB2 hash over "provider\0model\0prompt" as in the C runtime.
%% Uses unsigned 64-bit arithmetic (wraps at 2^64).
djb2_key(Provider, Model, Prompt) ->
    H0 = 5381,
    H1 = djb2_bytes(H0, binary_to_list(Provider)),
    H2 = djb2_byte(H1, 0),
    H3 = djb2_bytes(H2, binary_to_list(Model)),
    H4 = djb2_byte(H3, 0),
    H5 = djb2_bytes(H4, binary_to_list(Prompt)),
    %% Take unsigned 64-bit value (Erlang integers are arbitrary precision).
    H5 band 16#FFFFFFFFFFFFFFFF.

djb2_bytes(H, []) -> H;
djb2_bytes(H, [C | Rest]) ->
    djb2_bytes(djb2_byte(H, C), Rest).

djb2_byte(H, C) ->
    ((H * 33) bxor C) band 16#FFFFFFFFFFFFFFFF.

strip_trailing_newline(Bin) ->
    Size = byte_size(Bin),
    case Bin of
        <<Body:(Size-1)/binary, $\n>> -> Body;
        _ -> Bin
    end.
