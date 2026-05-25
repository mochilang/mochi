---
title: "Phase 0. Spec freeze and skeleton trees"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-45 Phase 0 tracking: spec freeze, transpiler3/c/ skeleton trees, implementation tracking pages, sidebar wiring."
---

# Phase 0. Spec freeze and skeleton trees

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 0](/docs/mep/mep-0045#phase-0-spec-freeze-and-skeleton-trees) |
| Status         | LANDED |
| Started        | 2026-05-22 (GMT+7) |
| Landed         | 2026-05-22 19:01 (GMT+7) |
| Tracking issue | [#22066](https://github.com/mochilang/mochi/issues/22066) |
| Tracking PR    | [#22067](https://github.com/mochilang/mochi/pull/22067) |

## Gate

This MEP merged on `main`; `transpiler3/c/{aotir,lower,emit,build,toolchain/zig,runtime/{include,src}}/doc.go` compile clean and report zero tests; `tests/transpiler3/c/` exists with a `README.md`; implementation tracking pages for every phase exist under `/docs/implementation/0045/`; sidebar entries visible on the website.

## Goal-alignment audit

The user-facing goal of MEP-45 is "ship a Mochi program as a single native binary on every tier-1 triple". Phase 0 does not move that goal directly; it is paperwork that costs one PR and unlocks every later phase. Specifically, after Phase 0 a contributor can answer four questions without reading the MEP end-to-end: (1) which Go package owns each pipeline stage (the `doc.go` tree), (2) where fixtures live and how they are named (`tests/transpiler3/c/README.md`), (3) which phase is responsible for which language surface (the §Phases table in MEP-45), and (4) where the per-phase rolling status lives (the tracking pages under `/docs/implementation/0045/`). Without those four anchors, every later phase repeats the same orientation cost. Aligns.

## Sub-phases

| #   | Scope                                                                                                     | Status      | Commit | PR |
|-----|-----------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 0.0 | MEP-45 merged with refactored framing, §Phases section, implementation tracking docs, sidebar wiring      | LANDED 2026-05-22 17:42 (GMT+7) | —      | [#22067](https://github.com/mochilang/mochi/pull/22067) |
| 0.1 | `transpiler3/c/{aotir,lower,emit,build,toolchain/zig,runtime/{include,src}}/doc.go` compile clean         | LANDED 2026-05-22 19:01 (GMT+7) | —      | [#22069](https://github.com/mochilang/mochi/pull/22069) |
| 0.2 | `tests/transpiler3/c/README.md` documents fixture layout and naming convention                            | LANDED 2026-05-22 19:01 (GMT+7) | —      | [#22069](https://github.com/mochilang/mochi/pull/22069) |

## Decisions made

- **Eight `doc.go` files, not one.** Each subpackage gets its own godoc-style narrative so that `go doc mochi/transpiler3/c/lower` is a useful first page for a contributor. The root `transpiler3/c/doc.go` carries only the pipeline diagram and cross-references.
- **`runtime/include/doc.go` and `runtime/src/doc.go` exist even though those dirs hold only C.** Keeping them as Go packages means `go vet ./transpiler3/c/...` walks the subtree without erroring and `go doc` surfaces the runtime layout next to the Go packages.
- **Fixture directory layout is phase-grouped.** `tests/transpiler3/c/fixtures/<phase-area>/<fixture-name>/` puts hello next to other Phase 1 fixtures, match next to other Phase 4 fixtures, and so on. The phase ownership is visible in the path; no extra registry file required.
- **Per-target expected output uses `expect.<triple>.txt` overrides.** Phase 11 needs cross-target output flexibility for rare cases (byte order, native int width). The harness picks the most-specific `expect.*.txt` match; default `expect.txt` covers everything else.

## Deferred work

- Per-phase substrate vendoring (BDWGC, mimalloc, minicoro, cwisstable, yyjson, libcurl, utf8proc / simdutf) is documented in `runtime/doc.go` but not yet imported. Each phase pulls in the substrate piece it needs (Phase 3 for cwisstable, Phase 9 for minicoro, Phase 14 for libcurl, ...).
- `transpiler3/c/toolchain/zig/manifest.go` SHA-256 pins are deferred to Phase 1.3, when the download path lands.

## Closeout notes

All 3 sub-phases landed. Phase 0 gate is green. PR #22069 delivered the complete skeleton; PR #22067 delivered the MEP framing.
