---
title: "Phase 14. fetch (HTTP)"
sidebar_position: 16
sidebar_label: "Phase 14. fetch (HTTP)"
description: "MEP-46 Phase 14. fetch (HTTP) — detailed implementation spec."
---

# Phase 14. fetch (HTTP)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 14. fetch (HTTP)](/docs/mep/mep-0046#phase-14-fetch-http) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's `fetch` expression on the BEAM target. `fetch` issues HTTP/HTTPS requests using OTP's `httpc` + `ssl` for connection and TLS, with JSON handled by OTP 27's built-in `json` stdlib module.

---

## Gate

See [MEP-46 §Phases · Phase 14. fetch (HTTP)](/docs/mep/mep-0046) for the normative gate. All 10 fixtures must produce byte-equal output to vm3. Fixtures run against a local test HTTP server started during test setup.

---

## Goal-alignment audit

`fetch` is the primary mechanism for Mochi programs to call external HTTP APIs, which is one of the most common real-world use cases. The 10 fixtures cover GET, POST, JSON encoding/decoding, TLS, concurrent requests, large responses, and mixed JSON types. All fixtures are directly user-visible. The per-host connection pool design ensures that production workloads with many requests to the same host do not pay TLS handshake overhead on every request.

---

## Sub-phases

### Sub-phase 14.0: mochi_fetch wrapping gun

**Syntax and lowering**

Simple GET:

```mochi
let resp = fetch "https://api.example.com/data"
```

Lowers to:

```erlang
c_call(c_atom(mochi_fetch), c_atom(get), [c_binary(<<"https://api.example.com/data">>)])
```

POST with options:

```mochi
let resp = fetch "https://api.example.com/items" with {
  method: "POST",
  body: payload,
  headers: { "Content-Type": "application/json" }
}
```

Lowers to:

```erlang
c_call(c_atom(mochi_fetch), c_atom(request), [
  c_map([
    {c_atom(method),  c_atom(post)},
    {c_atom(url),     c_binary(<<"https://api.example.com/items">>)},
    {c_atom(body),    V_payload},
    {c_atom(headers), c_map([{c_binary(<<"Content-Type">>), c_binary(<<"application/json">>)}])}
  ])
])
```

The lowerer converts the string method name `"POST"` to the atom `post` at compile time.

**mochi_fetch.erl public API**

```erlang
-spec get(binary()) -> {ok, response()} | {error, term()}.
get(Url) ->
  request(#{method => get, url => Url}).

-spec request(map()) -> {ok, response()} | {error, term()}.
request(Opts) ->
  {Host, Port, Path, Scheme} = parse_url(maps:get(url, Opts)),
  ConnPid = mochi_fetch_pool:get_or_create(Host, Port, Scheme),
  Method  = maps:get(method, Opts, get),
  Headers = maps:to_list(maps:get(headers, Opts, #{})),
  Body    = maps:get(body, Opts, <<>>),
  StreamRef = gun:request(ConnPid, method_to_binary(Method), Path, Headers, Body),
  await_response(ConnPid, StreamRef).
```

**Response type**

```erlang
-type response() :: #{
  status  := non_neg_integer(),
  headers := #{binary() => binary()},
  body    := binary()
}.
```

Non-2xx responses are returned as `{ok, Response}` with the actual status code in `response.status`. Only network-level errors (connection refused, DNS failure, TLS handshake failure) return `{error, Reason}`. Mochi programs check `resp.status` explicitly if they care about HTTP error codes. This follows the principle of least surprise: HTTP 404 is a valid HTTP response, not a protocol-level error.

**URL parsing**

`mochi_fetch:parse_url/1` uses `uri_string:parse/1` (OTP 23+ stdlib):

```erlang
parse_url(Url) ->
  #{scheme := Scheme, host := Host, path := Path} = uri_string:parse(Url),
  Port = case Scheme of
    <<"https">> -> 443;
    <<"http">>  -> 80
  end,
  {Host, Port, Path, Scheme}.
```

---

### Sub-phase 14.1: Per-host connection pooling via mochi_fetch_pool

**Architecture**

`mochi_fetch_sup.erl` is a `simple_one_for_one` supervisor. Each child is a `mochi_fetch_pool` gen_server managing one `gun` connection to one `{Host, Port, Scheme}` tuple.

```erlang
-module(mochi_fetch_sup).
-behaviour(supervisor).

init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
  ChildSpec = #{
    id      => mochi_fetch_pool,
    start   => {mochi_fetch_pool, start_link, []},
    restart => transient,
    type    => worker
  },
  {ok, {SupFlags, [ChildSpec]}}.
```

**mochi_fetch_pool.erl gen_server**

State:

```erlang
-record(state, {
  host     :: binary(),
  port     :: inet:port_number(),
  scheme   :: http | https,
  conn_pid :: pid() | undefined,
  pending  :: #{reference() => {pid(), reference()}}
}).
```

`get_or_create/3` looks up the pool by `{Host, Port, Scheme}` in an ETS table (`mochi_fetch_pools`). If no pool exists, it starts a new gen_server child under `mochi_fetch_sup` and registers it in the ETS table. Concurrent first-request races for the same host are serialized via a `mochi_fetch_registry` gen_server that owns the ETS write.

**Connection lifecycle**

On `init/1`, the pool gen_server calls:

```erlang
{ok, ConnPid} = gun:open(Host, Port, gun_opts(Scheme))
```

And waits for `{gun_up, ConnPid, Protocol}` in `handle_info` before accepting requests. If the connection drops (`gun_down`), the pool gen_server re-opens with exponential backoff (initial 100ms, max 30s, factor 2.0, jitter 10%).

**HTTP/2 multiplexing**

`gun` uses HTTP/2 by default for HTTPS connections (negotiated via ALPN in the TLS handshake). Multiple `gun:request/5` calls on the same `ConnPid` each return a unique `StreamRef`. Responses arrive as `{gun_response, ConnPid, StreamRef, fin, Status, Headers}` and `{gun_data, ConnPid, StreamRef, fin, Body}` messages. The pool gen_server correlates each `StreamRef` to its caller and replies via `gen_server:reply/2`.

---

### Sub-phase 14.1: TLS via ssl (OTP 27 TLS 1.3 default)

**gun TLS options**

`mochi_fetch_pool` constructs gun's TLS options for HTTPS connections:

```erlang
gun_opts(https) ->
  #{
    transport => tls,
    tls_opts  => [
      {versions,          ['tlsv1.3', 'tlsv1.2']},
      {verify,            verify_peer},
      {cacerts,           public_key:cacerts_get()},
      {customize_hostname_check, [
        {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
      ]}
    ]
  };
gun_opts(http) ->
  #{transport => tcp}.
```

Key TLS settings:

- `{versions, ['tlsv1.3', 'tlsv1.2']}` — explicitly excludes TLS 1.0 and TLS 1.1.
- `{verify, verify_peer}` — server certificate is verified against the CA bundle.
- `{cacerts, public_key:cacerts_get()}` — uses the system CA bundle (OTP 25+ API). On macOS, reads from the macOS trust store; on Linux, from `/etc/ssl/certs/ca-certificates.crt` or equivalent.
- `customize_hostname_check` with `pkix_verify_hostname_match_fun(https)` — performs RFC 6125-compliant hostname verification.

**Test mode: self-signed cert**

Test fixtures use a local HTTPS server. The test setup generates a self-signed certificate at test startup:

```erlang
setup_test_tls() ->
  {ok, _Cert, _Key} = mochi_test_tls:generate_self_signed(<<"localhost">>),
  application:set_env(mochi, fetch_tls_opts_override, [
    {verify, verify_none}
  ]).
```

`mochi_fetch_pool` checks for `fetch_tls_opts_override` in application env and substitutes the provided options. This override is only available when the `mochi` application is started with `{test_mode, true}` env; it is a compile-time error to set this override in a production release build.

---

### Sub-phase 14.2: JSON parse via stdlib json (OTP 27)

**JSON decode**

`json_body = json.decode(resp.body)` lowers to:

```erlang
c_call(c_atom(json), c_atom(decode), [V_body])
```

OTP 27's `json:decode/1` returns BEAM terms with the following mapping:

| JSON type | BEAM term |
|-----------|-----------|
| `null` | `null` atom |
| `true` | `true` atom |
| `false` | `false` atom |
| Number (integer) | BEAM integer |
| Number (float) | BEAM float |
| String | Binary (UTF-8) |
| Array | BEAM list |
| Object | BEAM map with binary keys |

**mochi_fetch:json/1 helper**

```erlang
-spec json(response()) -> term().
json(#{body := Body}) ->
  json:decode(Body).
```

This helper is lowered for the common pattern `let data = json(resp)` — the lowerer recognizes the `json(resp)` call pattern and emits `c_call(c_atom(mochi_fetch), c_atom(json), [V_resp])`.

**JSON encode**

For POST/PUT bodies:

```mochi
fetch url with { method: "POST", body: json_encode(payload) }
```

`json_encode` is a Mochi builtin that lowers to:

```erlang
c_call(c_atom(mochi_fetch), c_atom(json_encode), [V_payload])
```

```erlang
-spec json_encode(term()) -> binary().
json_encode(Term) ->
  iolist_to_binary(json:encode(Term)).
```

OTP 27's `json:encode/1` handles all BEAM native types that have JSON equivalents. It raises `badarg` for terms with no JSON representation (e.g., PIDs, references). The Mochi type checker prevents this at compile time for typed values; `any`-typed values passed to `json_encode` are checked at runtime.

**No external JSON deps**

OTP 27's `json` module provides encoding and decoding. For the Mochi runtime's needs (encoding request bodies, decoding response bodies), stdlib `json` is sufficient. Projects that need JSON path queries or streaming JSON can use FFI (Phase 12) to access `jsx`, `jiffy`, or other Hex packages.

---

## Test set

10 fixtures under `tests/transpiler3/beam/fixtures/phase14/`, run against a local test HTTP server:

| # | File | Description |
|---|------|-------------|
| 01 | `fetch_get_basic.mochi` | GET request; check status 200 |
| 02 | `fetch_get_json.mochi` | GET JSON endpoint; decode and access field |
| 03 | `fetch_post_json.mochi` | POST with JSON body; verify echo response |
| 04 | `fetch_headers.mochi` | Custom request headers sent; verify server received them |
| 05 | `fetch_404.mochi` | GET returns 404; Mochi program checks `resp.status` |
| 06 | `fetch_tls.mochi` | HTTPS GET against local TLS server with self-signed cert in test mode |
| 07 | `fetch_concurrent.mochi` | 10 concurrent `async (fetch ...)` calls, then `await_all` |
| 08 | `fetch_large_body.mochi` | Response body > 1 MB; verify full body received |
| 09 | `fetch_json_types.mochi` | JSON with mixed types (null, bool, int, float, string, array, object) |
| 10 | `fetch_post_form.mochi` | POST with `application/x-www-form-urlencoded` body |

The test helper `mochi_test_server.erl` starts a minimal Cowboy instance handling predefined routes. Cowboy is a test-only dep declared in `rebar.config` under the `test` profile:

```erlang
{profiles, [{test, [{deps, [{cowboy, "2.10.0"}]}]}]}.
```

---

## Decisions made

**Why per-host connection pooling in mochi_fetch_pool**

HTTP/1.1 keepalive and HTTP/2 multiplexing both require a persistent TCP connection per host. Without pooling, each `fetch` call would open a new TCP connection (3-way handshake: ~1ms LAN, ~100ms WAN) and complete a TLS handshake (~100ms additional for TLS 1.3 1-RTT). With a pooled HTTP/2 connection, these costs are paid once per host per application lifetime, and subsequent requests on the same host take ~1ms round trip. For a Mochi web service making many requests per second to the same upstream, the difference is significant.

**Why OTP 27 `json` stdlib instead of jsx/jiffy**

The OTP 27 `json` module was contributed by the OTP team and is maintained as part of the OTP release cycle, requiring no Hex.pm dependency, no NIF compilation, and no version pinning. `jiffy` is a NIF-based JSON library: NIF crashes can bring down the entire BEAM node (unlike Erlang process crashes which are isolated). `jsx` is a pure-Erlang library but adds a transitive Hex dep and its own versioning surface area. OTP 27's `json` is the correct choice for the Mochi runtime's zero-external-dep constraint on BEAM. For use cases requiring `json` performance beyond stdlib, users can access `jiffy` or `jason` (Elixir) via FFI (Phase 12).

**Why TLS 1.3 default and reject TLS 1.0/1.1**

TLS 1.0 and TLS 1.1 are deprecated by RFC 8996 (March 2021) and disabled by default in Chrome 84+, Firefox 78+, Safari 13+, and all major cloud load balancers. Accepting TLS 1.0/1.1 would expose Mochi programs to known attacks: BEAST (CVE-2011-3389) against TLS 1.0's CBC mode, POODLE (CVE-2014-3566) against SSL 3.0/TLS 1.0 fallback, and RC4 vulnerabilities present in older cipher suites. OTP 27's `ssl` application sets `['tlsv1.3', 'tlsv1.2']` as its default, but `mochi_fetch` makes this explicit in `gun_opts/1` to ensure the behaviour is not accidentally changed by OTP upgrades that might loosen defaults for compatibility reasons.

---

## Closeout notes

Sub-phases 14.0 and 14.1 (HTTP fetch via OTP `httpc` + `ssl`) landed together as `c3bb564682`. The implementation uses `httpc:request/4` from OTP's `inets` application rather than `gun`, keeping zero external Hex dependencies. Sub-phase 14.2 (`json_decode` via OTP 27's `json:decode/1`) landed as `f366d46f1f`. All 10 fixtures produce byte-equal output against vm3.
