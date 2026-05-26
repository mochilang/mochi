---
title: "Phase 13. LLM (generate)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM (generate)"
description: "MEP-46 Phase 13. LLM (generate) — detailed implementation spec."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 13. LLM (generate)](/docs/mep/mep-0046#phase-13-llm-generate) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's `generate` expression on the BEAM target. `generate` sends a prompt to a configured LLM provider and returns a structured response validated against a Mochi record schema. The runtime uses `gun` for HTTP/2 connections to provider APIs, a per-provider gen_server for connection pooling and request throttling, and a cassette-replay system for deterministic CI testing.

---

## Gate

See [MEP-46 §Phases · Phase 13. LLM (generate)](/docs/mep/mep-0046) for the normative gate. All 10 fixtures must produce byte-equal output to vm3. All 10 fixtures run against pre-recorded cassettes committed to the repo.

---

## Goal-alignment audit

`generate` is a first-class Mochi primitive for LLM-backed computation. The gate requires 10 fixtures covering basic generation, multi-field schemas, mixed types, prompt interpolation, and provider selection. All fixtures are user-facing. The cassette system ensures CI is deterministic without live API calls. Runtime schema validation catches provider errors at the point of generation rather than propagating untyped maps into Mochi code.

---

## Sub-phases

### Sub-phase 13.0: mochi_llm provider supervisor

**Architecture overview**

The LLM subsystem has three layers:

1. **`mochi_llm.erl`** — Public API module. Stateless; delegates to per-provider gen_servers.
2. **`mochi_llm_sup.erl`** — `one_for_one` supervisor. Starts one gen_server per configured provider.
3. **`mochi_llm_<provider>.erl`** — Provider-specific gen_server (e.g., `mochi_llm_openai`, `mochi_llm_anthropic`). Manages HTTP connection pool to the provider's API endpoint.

**Provider configuration**

Providers are configured at application start via `application:get_env/2`:

```erlang
%% In sys.config or application env:
{mochi, [
  {llm_providers, [
    {openai,    #{api_key => <<"sk-...">>, model => <<"gpt-4o">>}},
    {anthropic, #{api_key => <<"sk-ant-...">>, model => <<"claude-opus-4-5">>}}
  ]}
]}
```

If `llm_providers` is not set (or is `[]`), `mochi_llm_sup` starts no children. Calls to `mochi_llm:generate/2` with an unconfigured provider return `{error, provider_not_configured}`.

**mochi_llm_sup.erl**

```erlang
-module(mochi_llm_sup).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Providers = application:get_env(mochi, llm_providers, []),
  ChildSpecs = [provider_child_spec(Name, Opts) || {Name, Opts} <- Providers],
  SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
  {ok, {SupFlags, ChildSpecs}}.

provider_child_spec(Name, Opts) ->
  Module = provider_module(Name),
  #{
    id      => Name,
    start   => {Module, start_link, [Name, Opts]},
    restart => permanent,
    type    => worker
  }.

provider_module(openai)    -> mochi_llm_openai;
provider_module(anthropic) -> mochi_llm_anthropic;
provider_module(Other)     -> error({unknown_llm_provider, Other}).
```

**mochi_llm_openai.erl gen_server state**

```erlang
-record(state, {
  api_key   :: binary(),
  model     :: binary(),
  conn_pid  :: pid() | undefined,
  semaphore :: integer(),      %% remaining slots (max concurrent requests)
  max_conc  :: integer(),      %% configured max concurrent (default 10)
  pending   :: queue:queue()   %% queued requests waiting for a slot
}).
```

The gen_server handles:

- `handle_call({generate, Opts}, From, State)` — If a slot is available (semaphore > 0), fires the HTTP request via `gun` and stores `{From, StreamRef}` in the pending map. If the semaphore is 0, enqueues the request.
- `handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers}, State)` — Response received; deserialises JSON body; validates against schema; replies to caller.
- `handle_info({gun_down, ...}, State)` — Connection lost; reconnects with exponential backoff (initial 100ms, max 30s, factor 2.0).

Maximum concurrent requests (default 10) is configurable per-provider via `#{max_concurrent => N}` in the provider opts map.

**mochi_llm.erl public API**

```erlang
-spec generate(atom(), map()) -> {ok, map()} | {error, term()}.
generate(Provider, Opts) ->
  case mochi_llm_cassette:lookup(Provider, Opts) of
    {hit, Response} -> Response;
    miss ->
      Module = provider_module(Provider),
      Result = gen_server:call(Module, {generate, Opts}, 30000),
      mochi_llm_cassette:record(Provider, Opts, Result),
      Result
  end.
```

---

### Sub-phase 13.1: generate block lowering

**Syntax**

```mochi
let result = generate openai {
  prompt: "Summarize {text}",
  model: "gpt-4o",
  schema: { summary: string, word_count: int }
}
```

**Lowering steps**

The lowerer processes a `GenerateExpr` AST node through the following steps:

Step 1: **Prompt interpolation.** The prompt string `"Summarize {text}"` is lowered using the Phase 2.4 string interpolation pattern. If `text` is a Mochi variable holding a binary, the lowerer emits `Prompt = <<"Summarize ", Text/binary>>`. For complex interpolations with multiple variables, the lowerer emits `iolist_to_binary([...])`.

Step 2: **Schema lowering.** The Mochi record schema `{ summary: string, word_count: int }` is lowered to a BEAM map literal that represents the JSON Schema types:

```erlang
Schema = #{summary => <<"string">>, word_count => <<"integer">>}
```

The lowerer maps Mochi types to JSON Schema type strings: `int` -> `"integer"`, `float` -> `"number"`, `string` -> `"string"`, `bool` -> `"boolean"`.

Step 3: **generate call.** The lowerer emits:

```erlang
c_call(c_atom(mochi_llm), c_atom(generate), [
  c_atom(openai),
  c_map([
    {c_atom(prompt),  V_prompt},
    {c_atom(model),   c_binary(<<"gpt-4o">>)},
    {c_atom(schema),  V_schema}
  ])
])
```

Step 4: **Result unwrapping.** The `generate/2` call returns `{ok, #{summary => <<"...">>, word_count => 42}}`. The lowerer emits a pattern match to unwrap the `{ok, _}` tuple and bind the schema fields into the Mochi scope:

```erlang
{ok, Result} = mochi_llm:generate(openai, Opts),
Summary = maps:get(summary, Result),
WordCount = maps:get(word_count, Result)
```

**JSON Schema construction in the provider**

`mochi_llm_openai.erl` converts the Mochi schema map to an OpenAI-compatible JSON Schema:

```erlang
schema_to_json(Schema) ->
  Properties = maps:map(fun(_Key, TypeStr) ->
    #{<<"type">> => TypeStr}
  end, Schema),
  #{
    <<"type">>                 => <<"object">>,
    <<"properties">>           => Properties,
    <<"required">>             => maps:keys(Schema),
    <<"additionalProperties">> => false
  }.
```

This map is serialised to JSON via OTP 27's `json:encode/1` and sent in the `response_format` field of the OpenAI API request body.

**Response validation**

`mochi_llm.erl`'s `validate_response/2` checks that the returned map contains all expected keys with the correct runtime types:

```erlang
validate_response(Response, Schema) ->
  maps:foreach(fun(Key, TypeStr) ->
    Value = maps:get(Key, Response, undefined),
    case {Value, TypeStr} of
      {undefined, _}     -> error({schema_mismatch, missing_key, Key});
      {V, <<"string">>}  when not is_binary(V)  -> error({schema_mismatch, Key, expected_string});
      {V, <<"integer">>} when not is_integer(V) -> error({schema_mismatch, Key, expected_integer});
      {V, <<"number">>}  when not is_float(V), not is_integer(V) ->
        error({schema_mismatch, Key, expected_number});
      _ -> ok
    end
  end, Schema),
  {ok, Response}.
```

---

### Sub-phase 13.2: Replay-cassette test mode

**Cassette directory**

If the environment variable `MOCHI_LLM_CASSETTE_DIR` is set, `mochi_llm_cassette.erl` intercepts all `generate` calls. The cassette directory is read once at `mochi_app:start/2` and stored in application env.

**Cassette keying**

Each cassette file is named by the DJB2 hash of the tuple `{Provider, Prompt, Schema}` serialised to a canonical binary. DJB2 is chosen for its simplicity (no external dep) and low collision rate for short strings:

```erlang
djb2(Data) ->
  lists:foldl(fun(Byte, Hash) ->
    (Hash * 33 bxor Byte) band 16#FFFFFFFF
  end, 5381, binary_to_list(Data)).
```

Cassette files are stored as Erlang terms (`.eterms` extension) for human readability and easy manual editing:

```erlang
%% cassettes/1234567890.eterms
{cassette,
  provider, openai,
  prompt, <<"Summarize hello world">>,
  schema, #{summary => <<"string">>},
  response, {ok, #{summary => <<"Hello world is a greeting.">>}}
}.
```

**Cassette modes**

Two modes controlled by the `MOCHI_LLM_CASSETTE_MODE` env var:

- **`replay`** (default in CI): If a cassette file exists for the key, return its recorded response. If no cassette exists, return `{error, cassette_not_found}` (never makes a live API call).
- **`record`**: If a cassette file exists, return it (same as replay). If no cassette exists, make the live API call, save the response as a new cassette file, and return the result. Requires a live API key in the environment.

**mochi_llm_cassette.erl**

```erlang
lookup(Provider, Opts) ->
  case application:get_env(mochi, llm_cassette_dir) of
    undefined -> miss;
    {ok, Dir} ->
      Key = cassette_key(Provider, Opts),
      Path = filename:join(Dir, integer_to_list(Key) ++ ".eterms"),
      case file:consult(Path) of
        {ok, [{cassette, _, _, _, _, _, _, response, Response}]} -> {hit, Response};
        _ -> miss
      end
  end.
```

**CI cassette management**

All fixture cassettes are committed to the repository under `tests/transpiler3/beam/cassettes/`. CI always runs with:

```
MOCHI_LLM_CASSETTE_DIR=tests/transpiler3/beam/cassettes/
MOCHI_LLM_CASSETTE_MODE=replay
```

New cassettes are recorded locally by a developer with API keys and committed. The cassette directory is tracked in `.gitattributes` as binary to prevent line-ending normalisation on Windows.

---

## Test set

10 fixtures under `tests/transpiler3/beam/fixtures/phase13/`, all with pre-recorded cassettes:

| # | File | Description |
|---|------|-------------|
| 01 | `llm_summarize.mochi` | Summarize a short text, return `{summary: string}` |
| 02 | `llm_classify.mochi` | Classify sentiment, return `{sentiment: string}` |
| 03 | `llm_extract.mochi` | Extract name and age from text, return `{name: string, age: int}` |
| 04 | `llm_translate.mochi` | Translate English to French |
| 05 | `llm_multi_field.mochi` | Schema with 5 fields of mixed types |
| 06 | `llm_anthropic.mochi` | Same summarize task via Anthropic provider |
| 07 | `llm_schema_mismatch.mochi` | Provider returns wrong type; verify `schema_mismatch` error |
| 08 | `llm_prompt_interp.mochi` | Prompt with variable interpolation |
| 09 | `llm_nested_schema.mochi` | Schema with `map<string, string>` field |
| 10 | `llm_async_generate.mochi` | `async (generate ...)` combined with Phase 11 |

---

## Decisions made

**Why gun for HTTP in the LLM provider**

OTP's built-in `httpc` (from the `inets` application) uses a single process per connection pool, making concurrent requests serialised per pool. For LLM use cases where a Mochi program may issue 10 or more concurrent `generate` calls, `httpc` becomes a bottleneck. `gun` supports HTTP/2 stream multiplexing: 50 concurrent requests share a single TCP connection, with BEAM receiving response frames asynchronously via `handle_info` callbacks. `gun` is also already a dependency for `mochi_fetch` (Phase 14), so using it here adds no new transitive deps.

**Why schema validation in the runtime rather than compile-time**

LLM responses are dynamically generated by a remote model; the schema is a hint to the provider (via JSON Schema `response_format` in OpenAI's API) but not a hard guarantee. The model may hallucinate fields, omit required fields, or return a field with the wrong type. Mochi's type checker validates that the schema expression is syntactically correct and that call sites use the correct field names and types (compile-time safety). But the actual content of the response can only be validated at runtime. `mochi_llm.erl`'s `validate_response/2` performs this runtime check and returns `{error, {schema_mismatch, Key, Reason}}` on failure, allowing Mochi programs to handle validation errors gracefully rather than crashing on a bad pattern match.

---

## Closeout notes

_Fill in after gate green._
