%% mochi_llm: LLM generation for the Mochi BEAM runtime.
%%
%% Phase 13.0: cassette-backed LLM generation.
%% Phase 13.2: live provider calls (OpenAI, Anthropic) via httpc.
%%
%% Dispatch order:
%%   1. MOCHI_LLM_CASSETTE_DIR set  → cassette playback
%%   2. OPENAI_API_KEY set          → OpenAI chat/completions
%%   3. ANTHROPIC_API_KEY set       → Anthropic messages
%%   4. neither                     → warning + empty binary
-module(mochi_llm).
-export([generate/3]).

%% generate(Provider, Model, Prompt) -> binary()
%% Provider, Model, Prompt are UTF-8 binaries.
generate(Provider, Model, Prompt) ->
    case os:getenv("MOCHI_LLM_CASSETTE_DIR") of
        false ->
            live_generate(Provider, Model, Prompt);
        Dir ->
            cassette_lookup(Dir, Provider, Model, Prompt)
    end.

%% live_generate: route to a real LLM provider using env-var API keys.
live_generate(_Provider, Model, Prompt) ->
    case os:getenv("OPENAI_API_KEY") of
        false ->
            case os:getenv("ANTHROPIC_API_KEY") of
                false ->
                    io:format(standard_error,
                        "mochi_llm: no API key found (set OPENAI_API_KEY or ANTHROPIC_API_KEY)~n", []),
                    <<>>;
                AnthropicKey ->
                    openai_or_anthropic(anthropic, Model, Prompt, AnthropicKey)
            end;
        OpenAIKey ->
            openai_or_anthropic(openai, Model, Prompt, OpenAIKey)
    end.

openai_or_anthropic(openai, Model, Prompt, Key) ->
    ok = ensure_httpc(),
    EModel = case Model of
        <<>> -> <<"gpt-4o-mini">>;
        _    -> Model
    end,
    Body = json_encode_openai(EModel, Prompt),
    Hdrs = [{"Authorization", "Bearer " ++ Key},
            {"Content-Type", "application/json"}],
    case httpc:request(post,
            {"https://api.openai.com/v1/chat/completions", Hdrs,
             "application/json", Body},
            [{ssl, [{verify, verify_peer},
                    {cacerts, public_key:cacerts_get()}]}],
            []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            parse_openai_response(list_to_binary(RespBody));
        {ok, {{_, Code, _}, _, RespBody}} ->
            io:format(standard_error,
                "mochi_llm: OpenAI HTTP ~p: ~s~n", [Code, RespBody]),
            <<>>;
        {error, Reason} ->
            io:format(standard_error,
                "mochi_llm: OpenAI request error: ~p~n", [Reason]),
            <<>>
    end;
openai_or_anthropic(anthropic, Model, Prompt, Key) ->
    ok = ensure_httpc(),
    EModel = case Model of
        <<>> -> <<"claude-haiku-4-5-20251001">>;
        _    -> Model
    end,
    Body = json_encode_anthropic(EModel, Prompt),
    Hdrs = [{"x-api-key", Key},
            {"anthropic-version", "2023-06-01"},
            {"Content-Type", "application/json"}],
    case httpc:request(post,
            {"https://api.anthropic.com/v1/messages", Hdrs,
             "application/json", Body},
            [{ssl, [{verify, verify_peer},
                    {cacerts, public_key:cacerts_get()}]}],
            []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            parse_anthropic_response(list_to_binary(RespBody));
        {ok, {{_, Code, _}, _, RespBody}} ->
            io:format(standard_error,
                "mochi_llm: Anthropic HTTP ~p: ~s~n", [Code, RespBody]),
            <<>>;
        {error, Reason} ->
            io:format(standard_error,
                "mochi_llm: Anthropic request error: ~p~n", [Reason]),
            <<>>
    end.

%% json_encode_openai: minimal JSON for {"model":M,"messages":[{"role":"user","content":P}]}
json_encode_openai(Model, Prompt) ->
    io_lib:format(
        "{\"model\":~s,\"messages\":[{\"role\":\"user\",\"content\":~s}]}",
        [json_string(Model), json_string(Prompt)]).

%% json_encode_anthropic: minimal JSON for Anthropic messages API.
json_encode_anthropic(Model, Prompt) ->
    io_lib:format(
        "{\"model\":~s,\"max_tokens\":1024,\"messages\":[{\"role\":\"user\",\"content\":~s}]}",
        [json_string(Model), json_string(Prompt)]).

%% parse_openai_response: extract choices[0].message.content from decoded JSON.
parse_openai_response(Bin) ->
    try
        Map = json:decode(Bin),
        Choices = maps:get(<<"choices">>, Map),
        First = hd(Choices),
        Msg = maps:get(<<"message">>, First),
        Content = maps:get(<<"content">>, Msg),
        if is_binary(Content) -> Content;
           true -> iolist_to_binary(io_lib:format("~p", [Content]))
        end
    catch _:_ ->
        io:format(standard_error,
            "mochi_llm: failed to parse OpenAI response~n", []),
        <<>>
    end.

%% parse_anthropic_response: extract content[0].text from decoded JSON.
parse_anthropic_response(Bin) ->
    try
        Map = json:decode(Bin),
        Content = maps:get(<<"content">>, Map),
        First = hd(Content),
        Text = maps:get(<<"text">>, First),
        if is_binary(Text) -> Text;
           true -> iolist_to_binary(io_lib:format("~p", [Text]))
        end
    catch _:_ ->
        io:format(standard_error,
            "mochi_llm: failed to parse Anthropic response~n", []),
        <<>>
    end.

%% json_string: JSON-encode a binary as a quoted string with basic escaping.
json_string(Bin) ->
    Escaped = binary:replace(
        binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
        <<"\"">>, <<"\\\"">>, [global]),
    [$", Escaped, $"].

%% ensure_httpc: start inets + ssl if not already running.
ensure_httpc() ->
    _ = application:ensure_all_started(inets),
    _ = application:ensure_all_started(ssl),
    ok.

%% cassette_lookup: look up DJB2-keyed cassette file.
cassette_lookup(Dir, Provider, Model, Prompt) ->
    Hash = djb2_key(Provider, Model, Prompt),
    HashStr = integer_to_list(Hash),
    Path = filename:join(Dir, HashStr ++ ".txt"),
    case file:read_file(Path) of
        {ok, Data} ->
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
