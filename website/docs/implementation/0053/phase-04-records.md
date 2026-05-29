---
title: "Phase 4. Records"
sidebar_position: 6
sidebar_label: "Phase 4. Records"
description: "MEP-53 Phase 4, record / anonymous-record types lowered to Rust structs with #[derive(Clone, Debug, PartialEq, Default)]."
---

# Phase 4. Records (structs)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | a7b47981ff |

## Gate

`TestPhase4Records` walks `tests/transpiler3/rust/fixtures/phase04-records/` (14 fixtures) and asserts byte-equal stdout. Coverage: record literal, field access, field update (functional, via struct update syntax `{ ..base, field: new }`), nested records, anonymous-record types, records in lists / maps, structural equality, print via Debug.

## Lowering decisions

`record User { id: int, name: string }` and `type Pair = { a: int, b: int }` both lower to:

```rust
#[derive(Clone, Debug, PartialEq, Default)]
struct User {
    id: i64,
    name: String,
}
```

Both surface forms route through `lowerStruct` in `transpiler3/rust/lower/struct.go`. The `Default` derive lets `spawn AgentType()` (phase 9) construct zero-valued instances. The `PartialEq` derive lets `==` work as expected. The `Clone` derive is required because field updates are functional (`{ ..base, field: new }` consumes `base` by value unless cloned). The `Debug` derive is required for `print(user)` to lower to `mochi_runtime::io::print_str(format!("{:?}", user))`.

Field updates lower to:

```rust
User { id: 42, ..base.clone() }
```

The `.clone()` on `base` is gated by the colour pass; in phase 4 it always clones, in phase 6 it elides where the base is dead.

Anonymous-record types declared at use site (`let p: { a: int, b: int } = { a: 1, b: 2 }`) get a synthesised struct name (`__Anon_<sha8>`) hoisted to module scope; deduplication is by structural-hash of the field set, so two uses of the same anonymous shape collapse to the same struct.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/struct.go` | Record + anonymous-record lowering |
| `transpiler3/rust/build/phase04_test.go` | 14-fixture gate |
| `tests/transpiler3/rust/fixtures/phase04-records/*.mochi` + `.out` | 14 fixtures |

## Test set

- `TestPhase4Records/<fixture>` for each `.mochi` in the fixture directory (14 fixtures).

## Closeout notes

Printing records via Rust's `{:?}` Debug format does not match vm3's pretty-printer exactly (vm3 separates fields with `, `, Debug with `,`; vm3 elides type name on anonymous records). Phase 4 fixtures all stay within field-by-field printing rather than whole-struct printing to dodge the gap; a follow-up sub-phase could emit a custom `Display` derive that matches vm3 byte-for-byte.
