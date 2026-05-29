---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "Risk register (BEAM abstract code drift, EDoc coverage gaps, Port latency, rebar3 fragility, Hex.pm API stability, ok/error false positives, WASM exclusion, OTP version matrix) and rejected alternatives (NIF default, Dialyzer PLT primary, C-node bridge, HEX_API_KEY publish, full Elixir, custom resolver)."
---

# 12. Risks and alternatives

## Risk register

### R1: BEAM abstract code format drift across OTP versions

**Risk**: The `Dbgi` chunk format is documented as internal and could change across OTP major versions. A new OTP release might introduce a new debug-info backend or change the ETF layout of the abstract syntax tree.

**Likelihood**: Low. The `Dbgi` chunk has been stable since OTP 20 (2017). The `erl_abstract_code` backend is the default and the OTP core team has not announced plans to change it. The `Abst` fallback (OTP 17-19) provides backward compatibility.

**Mitigation**: The `beam-ingest-sha256` in `mochi.lock` anchors the ingest to the exact chunk bytes. If the format changes, `mochi pkg lock --check` will fail, alerting the user to re-run `mochi pkg lock`. The bridge tests include a golden-file test suite that records expected abstract code AST shapes for the 20-package fixture corpus on OTP 25 and OTP 27. A CI job runs this suite on each new OTP minor release.

### R2: EDoc coverage gaps

**Risk**: Packages in the fixture corpus that the bridge classifies as `edoc`-fallback (lager, observer_cli, cuttlefish) may not ship pre-generated EDoc XML in their Hex.pm tarballs. In that case, the bridge falls back to `no_typeinfo`, and the package produces a blanket SkipReport.

**Likelihood**: Medium. Older Erlang packages often skip documentation tooling. lager in particular has historically been maintained with minimal documentation infrastructure.

**Mitigation**: Phase 3 gate includes a test that generates EDoc XML from the package source files using a bundled `edoc` invocation (requiring OTP on PATH). If the pre-generated XML is not in the tarball, the bridge runs `erl -eval 'edoc:run(...)' -s init stop` to generate it. This requires OTP at `mochi pkg lock` time for `edoc`-fallback packages only. The requirement is documented in the SkipReport and in the `mochi pkg lock` output.

### R3: rebar3 version fragility

**Risk**: The rebar3 fallback resolver (tier 2) generates a `rebar.config` and invokes `rebar3 upgrade`. If the user's installed rebar3 version does not match the `[erlang].rebar3-version` constraint, the fallback produces a different lock than expected.

**Likelihood**: Low. The bridge enforces the `rebar3-version` constraint at the start of every `mochi pkg lock` run. Users who have an incompatible rebar3 installed get a clear error message with installation instructions.

**Mitigation**: The bridge ships a `mochi tool install rebar3 <version>` helper that downloads the rebar3 escript from GitHub Releases and installs it into `~/.cache/mochi/tools/rebar3/<version>/`. If the system rebar3 does not satisfy the constraint, the bridge uses the bundled version automatically.

### R4: Complex OTP callback patterns escaping the behavior heuristic

**Risk**: Some OTP libraries expose complex behavior hierarchies (e.g., a module that is both a `gen_server` and exports a custom behavior that other modules implement). The behavior heuristic (§10) may misclassify such modules and expose internal callback functions in the `extern fn` corpus.

**Likelihood**: Low for the fixture corpus; possible for unusual packages in the wild.

**Mitigation**: A `SkipReport: behavior_callback_leaked` entry is added when the bridge detects a suspected callback function (by matching against the known OTP callback signatures: `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`, `code_change/3`). The user can suppress the entry by adding an explicit `[erlang.skip]` annotation in `mochi.toml`.

### R5: Hex.pm OIDC API stability

**Risk**: Hex.pm launched trusted publishing in 2024. The API is relatively new and could change.

**Likelihood**: Low. The Hex.pm team has committed to the OIDC flow as the primary publishing path. The API follows the same JWT + OIDC provider pattern as PyPI, npm, and RubyGems.org, which are all stable. The bridge will track Hex.pm API changes as part of the normal maintenance cycle.

**Mitigation**: The `mock-hex` harness tests the publish flow end-to-end without contacting the live registry, providing a regression test suite that is independent of live API availability.

### R6: ok/error pattern false positives

**Risk**: The ok/error idiom recogniser might match a 2-tuple union that is structurally identical to `{ok, T} | {error, Reason}` but semantically different (e.g., a `{get, Key} | {set, Value}` tuple used as a command union). In that case, the bridge would produce an incorrect type translation.

**Likelihood**: Very low. The ok/error pattern recogniser checks specifically for `{atom, _, ok}` and `{atom, _, error}` as the first element of each branch. A `{get, Key} | {set, Value}` pattern has `get` and `set` atoms, not `ok` and `error`, and would not be matched.

**Mitigation**: The bridge adds a `SkipNote: ok_error_pattern_applied` annotation to every translation that uses the pattern recogniser. The user can inspect the generated `shim.mochi` file to verify the translation is correct. A hand-authored `extern fn` override suppresses the generated binding.

### R7: WASM target exclusion

**Risk**: Mochi programs targeting WASM cannot use `import erlang "..."` because `open_port/2` requires OS process spawning. This silently prevents the bridge from working in an important target environment.

**Likelihood**: Certain (by design, not a bug).

**Mitigation**: The bridge driver asserts the WASM exclusion at compile time: if the active Mochi target is WASM (`-target wasm32`), any `import erlang "..."` statement produces a compile error with a clear diagnostic. The error message suggests using a non-WASM target or replacing the import with a hand-written HTTP client that calls an Erlang service over a network boundary.

### R8: OTP version matrix complexity

**Risk**: The bridge supports OTP 25+ (for `Dbgi` stability) on linux-x64 and darwin-arm64. Testing across multiple OTP versions in CI doubles the test matrix size and increases maintenance burden.

**Likelihood**: Moderate ongoing cost.

**Mitigation**: The CI matrix tests phases 0-5 (ingest + type mapping) on both OTP 25 and OTP 27. Phases 6-13 (runtime bridge) are tested only on OTP 27. OTP 25 is the minimum supported version; issues that appear only on OTP 25 are treated as high-priority bugs.

## Rejected alternatives

### A1: NIF as default bridge mechanism

**Rejected because**: A crashing NIF kills the entire BEAM VM. Any bug in the generated NIF (and there will be bugs; the generated NIF would call into arbitrary Erlang library code via `erl_interface`) would crash the user's production node. Port isolation prevents this. NIF performance (~100x faster than Port IPC) is not needed for the I/O-bound library functions that dominate the fixture corpus.

### A2: Dialyzer PLT as primary type source

**Rejected because**: Building a PLT requires running `dialyzer --build_plt` on the package's `.beam` files, which requires OTP on PATH and takes 10-60 seconds per package. The PLT format is binary and OTP-version-specific: a PLT built on OTP 25 is not directly comparable to one built on OTP 27. BEAM abstract code is in-artifact, requires no tool invocation, and is OTP-version-stable for the relevant chunk formats.

### A3: C-node as default bridge mechanism

**Rejected because**: C-node setup requires EPMD, a node cookie, and a network connection. In a development environment where the user just wants to call `hackney:get/4` from a script, EPMD registration is unnecessary overhead. C-node is appropriate for phase 13 (distributed bridge) where the Mochi binary must appear as a named node in a cluster.

### A4: HEX_API_KEY as publish path

**Rejected because**: Long-lived API tokens are the primary supply-chain attack vector. See [[07-oidc-trusted-publishing]] for the full rationale.

### A5: Full Elixir support in scope

**Rejected because**: Elixir libraries that use Elixir-specific features (protocols, macros, `__struct__`, `Enum.map/2`, `String.t()`) require the Elixir runtime and Elixir's standard library. Supporting them would require either bundling the Elixir runtime in the bridge or restricting to a subset of Elixir that compiles to plain Erlang modules. The first option is prohibitively complex; the second is indistinguishable from "Erlang-compatible Elixir packages," which is already in scope via the `elixir-compat` flag. Full Elixir is a future MEP.

### A6: Custom Erlang resolver in Go

**Rejected because**: Writing a Go-side resolver that correctly handles all of rebar3's edge cases (override, umbrella apps, hex lockfiles, git sources, path sources, minimum_otp_vsn constraints, platform-specific deps, profile-level deps) would require 5,000+ LOC and perpetual maintenance. The two-tier strategy (Go resolver for common cases, rebar3 fallback for complex cases) minimises the Go-side resolver scope while ensuring correctness.

## Cross-references

- [[02-design-philosophy]] for the Port vs NIF rationale.
- [[07-oidc-trusted-publishing]] for the HEX_API_KEY prohibition rationale.
- [[08-port-bridge-protocol]] for the Port latency profile.
- [[11-version-resolution]] for the two-tier resolver design.
