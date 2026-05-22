---
title: MEP-46 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-46 (Mochi-to-Erlang/BEAM transpiler). Twelve notes covering language surface, BEAM design philosophy, prior art on BEAM languages, OTP runtime, Core Erlang codegen, type lowering, portability, dataset pipeline, agents/streams, build system, testing, and risks."
---

# MEP-46 research notes

These twelve notes are the deep research that fed MEP 46 (Mochi-to-Erlang/BEAM transpiler). They are informative; the MEP body, once landed, will be normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0046/language-surface) | Mochi features mapped onto BEAM lowering obligations |
| 02 | [Design philosophy](/docs/research/0046/design-philosophy) | Why BEAM, why Core Erlang, why OTP wholesale |
| 03 | [Prior-art transpilers](/docs/research/0046/prior-art-transpilers) | Elixir, Gleam, LFE, Hamler, Alpaca, Clojerl, AtomVM, OTP internals |
| 04 | [Runtime building blocks](/docs/research/0046/runtime) | gen_server, supervisor, pg, ETS, gun, json, telemetry |
| 05 | [Codegen design](/docs/research/0046/codegen-design) | Core Erlang via `cerl`, `compile:forms/2` with `from_core` |
| 06 | [Type-system lowering](/docs/research/0046/type-lowering) | Type-by-type mapping to BEAM terms |
| 07 | [Erlang target and portability](/docs/research/0046/erlang-target-portability) | OTP 27/28 matrix, arch matrix, AtomVM profile |
| 08 | [Dataset pipeline lowering](/docs/research/0046/dataset-pipeline) | Query DSL to comprehensions/foldl, Datalog to ETS + semi-naive |
| 09 | [Agents and streams on OTP](/docs/research/0046/agent-streams) | agents to gen_server, streams to pg, async to monitored spawn |
| 10 | [Build system](/docs/research/0046/build-system) | rebar3, mix, escript, OTP release, AtomVM packaging |
| 11 | [Testing and CI gates](/docs/research/0046/testing-gates) | Per-phase gates, Dialyzer cleanliness, OTP version matrix |
| 12 | [Risks and alternatives](/docs/research/0046/risks-and-alternatives) | Risk register, alternatives rejected |
