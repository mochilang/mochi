---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "Rustler (Rust NIF), Zigler (Zig NIF), erlport (Python/Ruby Port), JInterface (Java C-node), Ports vs NIFs vs C-nodes taxonomy, and what MEP-66 borrows from each."
---

# 03. Prior-art bridges

## Rustler

Rustler is the most widely used Erlang/Elixir foreign-language bridge as of 2026. It generates Erlang NIFs from Rust functions using a Rust proc-macro (`#[rustler::nif]`) and a Cargo build step (`mix compile` or `rebar3 compile` with a custom hook). The user writes Rust functions annotated with `#[rustler::nif]`, runs `mix compile`, and calls the functions from Elixir/Erlang as though they were native module functions.

**What Rustler gets right**: The developer experience is excellent. The `#[rustler::nif]` macro handles ETF encoding/decoding for the most common Erlang types (integers, floats, binaries, atoms, lists, tuples, maps, pids). The compile-time check that the Rust function signature matches what the NIF framework expects catches type mismatches early. The `rustler_elixir_fun` crate adds support for passing Elixir/Erlang anonymous functions to Rust NIFs.

**What Rustler requires**: The user must write Rust. Every bridged function requires a manually-written NIF wrapper. Rustler does not discover functions from a library's public API; it requires explicit `#[rustler::nif]` annotation on every function the user wants to expose. There is no automatic type translation table or `SkipReport`.

**What MEP-66 borrows from Rustler**: The ETF type encoding/decoding strategy (atom ↔ string, binary ↔ bytes, list ↔ list, tuple ↔ tuple or record). The principle that safety-critical paths (atoms, pids, references) should be opaque handles on the Mochi side rather than translated to primitives.

**Why MEP-66 does not use NIF by default**: Rustler's NIF approach requires the user to write Rust and to annotate every function. MEP-66's goal is zero user-written boilerplate. More importantly, Rustler's NIF approach inherits NIF's crash-kills-VM problem: a bug in the generated NIF takes down the user's BEAM node. MEP-66 uses Port IPC as the default to eliminate this risk.

## Zigler

Zigler is a Zig-based NIF generator for Elixir, similar to Rustler but targeting the Zig programming language. It uses Zig's `comptime` reflection to generate NIF boilerplate from Zig function signatures at compile time. The `zigtest` harness enables unit testing of Zig functions before they are compiled as NIFs.

**What MEP-66 borrows from Zigler**: The idea of using the target language's own type system metadata (Zig's `@typeInfo`, in MEP-66's case Erlang's `-spec`) to drive automatic wrapper generation, rather than requiring hand-written wrapper code.

## erlport

erlport is a library that bridges Erlang (and Elixir) to Python and Ruby using OTP Ports and the ETF encoding. The Erlang side uses `python:call/3` or `ruby:call/3` to invoke a function in a running Python/Ruby interpreter. The Python/Ruby side uses a `Port` process (an `erlport` runtime) that sits on stdin/stdout, decodes ETF, dispatches to a Python/Ruby function, and encodes the result back.

**What erlport gets right**: erlport proves that the Port + ETF approach is viable for bridging Erlang to higher-level languages. The latency profile (50-200 microseconds per call for simple operations) is documented and acceptable for I/O-bound use cases.

**What erlport requires**: The user must write Python/Ruby functions that accept and return ETF-compatible types. erlport does not read Python/Ruby type annotations; it maps types at the ETF layer without any static type checking.

**What MEP-66 borrows from erlport**: The Port + ETF framing model (`{packet, 4}` with `term_to_binary`/`binary_to_term` on both sides). The call/response message schema `{call, Fun, Args}` / `{ok, Result}` / `{error, Reason}`. The gen_server wrapping pattern that gives the Port a supervised, named-process identity in the OTP application.

**Key difference from erlport**: MEP-66 flows in the opposite direction (Mochi calls Erlang, not Erlang calls Mochi) and adds static type mapping from BEAM abstract code to Mochi `extern fn` declarations, which erlport does not.

## JInterface

JInterface is Ericsson's Java library for participating as an Erlang C-node. It provides Java classes for decoding and encoding Erlang terms (ETF), connecting to an Erlang node via the OTP distribution protocol, sending and receiving Erlang messages, and registering named processes in the Erlang process registry.

**What JInterface gets right**: The C-node/distribution protocol approach allows a non-BEAM process to be a first-class member of an Erlang cluster, receiving `gen_server:call/2` messages and participating in OTP supervision from remote nodes. This is the highest-fidelity integration path.

**What JInterface requires**: EPMD (the Erlang Port Mapper Daemon) must be running. The Java process must authenticate with the cluster's cookie. The setup is non-trivial for development environments where the user just wants to call a library function from a script.

**What MEP-66 borrows from JInterface**: The C-node approach is the model for MEP-66 phase 13 (distributed Erlang bridge). The `erl_interface` C library (the C equivalent of JInterface) provides the node-connection and message-routing functions. The Go-side C-node implementation in `package3/erlang/cnode/` wraps `erl_interface` via cgo.

## Ports vs NIFs vs C-nodes: taxonomy

| Mechanism | Isolation | Latency | Setup cost | Crash behaviour | OTP supervision |
|-----------|----------|---------|-----------|-----------------|----------------|
| Port (`{packet,4}`) | Full (OS process) | 50-200 µs | Minimal | Port restart; VM safe | Full (supervisor + monitor) |
| NIF (shared lib) | None (in-process) | 0.1-1 µs | Build toolchain | VM crash | None |
| C-node (`erl_interface`) | Full (OS process) | 100-500 µs | EPMD + cookie + node name | Node disconnect; VM safe | Partial (net_kernel monitor) |

MEP-66 phases 0-12 use Ports. Phase 13 uses C-node. A future N.1 sub-phase may offer NIF opt-in.

## What MEP-66 does not borrow

**wx**: Erlang's built-in wxWidgets binding is generated from the wxWidgets C++ class hierarchy, not from a type declaration. The approach is one-off and not generalisable.

**gen_binding**: A research project that generates Erlang gen_server stubs from OpenAPI specs. The model of generating bindings from a machine-readable API description is analogous, but OpenAPI is not available for arbitrary Erlang packages.

**Otter**: A distributed tracing library for Erlang that bridges to Zipkin via Port IPC. The Port pattern is the same as MEP-66; the bridge does not contribute type-level features.

## Cross-references

- [[02-design-philosophy]] for the Port vs NIF decision rationale.
- [[08-port-bridge-protocol]] for the Port + ETF protocol detail.
- [[12-risks-and-alternatives]] for the rejected NIF-default and C-node-default alternatives.
