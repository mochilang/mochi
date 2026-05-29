---
title: "Phase 24. TargetIRubyKernel"
sidebar_position: 28
sidebar_label: "Phase 24. TargetIRubyKernel"
description: "MEP-56 Phase 24, emit a Jupyter notebook (.ipynb) with the lowered Ruby as a single code cell and an IRuby kernelspec."
---

# Phase 24. TargetIRubyKernel

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 72522b6311 |

## Gate

`TestPhase24TargetIRubyKernel` in `transpiler3/ruby/build/phase24_test.go`: builds `hello_nb.mochi` (`print("hi from notebook")\nprint(2 + 2)`) via `Driver.Build` with `TargetIRubyKernel`, reads the resulting `hello_nb.ipynb`, asserts `json.Unmarshal` succeeds (the file is valid JSON), asserts `nbformat == 4`, asserts `metadata.kernelspec.name == "ruby"`, `kernelspec.language == "ruby"`, and `language_info.name == "ruby"`, asserts there is exactly one cell with `cell_type == "code"`, asserts the joined cell source contains both `hi from notebook` and `frozen_string_literal`, and asserts every source line except the last ends in a `\n` (the nbformat 4 source-array convention).

## Build target / audit decisions

`buildIRubyNotebook` in `transpiler3/ruby/build/build.go` (lines 296 to 335) marshals a `map[string]any` with the nbformat 4 schema: top-level `nbformat: 4`, `nbformat_minor: 5`, a `metadata` object pinning the kernel to `name: "ruby"` / `display_name: "Ruby (IRuby)"` / `language: "ruby"` with `language_info.name: "ruby"` and `file_extension: ".rb"`, and a single code cell with `execution_count: nil`, empty `metadata`, empty `outputs`, and `source` populated from `splitKeepNewline(sf.RubySource())`.

`splitKeepNewline` (lines 498 to 507) splits the source on `\n` but keeps the trailing newline on each non-final line, matching the Jupyter convention where `source` is an array of strings and each except the last ends in `"\n"`. The convention exists so notebook merge tools can diff line by line. Concatenating the array with the empty string reconstructs the original file byte for byte.

`json.MarshalIndent(nb, "", " ")` (single-space indent) keeps the file compact while staying human-readable; nbformat does not require pretty printing, but most notebook tooling assumes it. The kernel is pinned to `ruby` (not `iruby`) because the IRuby project registers itself under the bare `ruby` kernel name via `iruby register`, matching what `jupyter kernelspec list` reports.

The build emits a single code cell, not a notebook split into per-statement cells. The motivation: every Mochi source has exactly one main module and one entry point, so a multi-cell split would have to decide arbitrarily where to break. A single cell preserves whole-file semantics and lets the user manually split it after opening the notebook if they want a presentation layout.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildIRubyNotebook` writes `<name>.ipynb` with nbformat 4 + IRuby kernelspec (lines 296 to 335); `splitKeepNewline` helper (lines 498 to 507); `Driver.Build` dispatches `TargetIRubyKernel` (line 209) |
| `transpiler3/ruby/build/phase24_test.go` | `TestPhase24TargetIRubyKernel` JSON unmarshal + nbformat / kernelspec / cell-shape assertions |

## Test set

- `TestPhase24TargetIRubyKernel` (single test, no subtests): asserts the file is valid JSON, `nbformat == 4`, kernelspec / language_info pin to `ruby`, one code cell, source contains the print literal and `frozen_string_literal`, and the line-ending convention holds.

## Closeout notes

Phase 24 validates the notebook by JSON-decoding into a typed struct rather than by substring match. This catches malformed JSON (trailing commas, unquoted keys) that a substring check would miss, and the typed struct surfaces schema drift at unmarshal time. The trailing-newline assertion at the end of the test is what proved most useful in practice: an earlier draft used `strings.Split` (no trailing `\n` on the split parts) and the notebook still validated but `JSON.parse` round-trips showed concatenation gaps. Switching to `strings.SplitAfter` and trimming a possibly-empty final element produces the exact array shape Jupyter expects.
