// Package runtime documents the libmochi C runtime that ships
// linked into every binary produced by transpiler3/c.
//
// Design contract: MEP-45 §3 "Runtime (libmochi)" and §10
// "Vendored substrate". See website/docs/mep/mep-0045.md.
//
// This directory contains C, not Go. The doc.go here exists so
// that go vet ./transpiler3/c/... walks the subtree and so the
// runtime layout is discoverable from godoc.
//
// Layout:
//
//	runtime/include/mochi/*.h    public API consumed by emit
//	runtime/src/*.c              implementation
//	runtime/third_party/*        vendored deps with SHA pins
//
// Substrate (vendored, statically linked, MEP-45 §10):
//
//   - BDWGC                Boehm-Demers-Weiser conservative GC.
//   - mimalloc             allocator backing the GC.
//   - minicoro             stackful fibers for streams + agents.
//   - cwisstable           Swiss tables (open-addressed hash).
//   - yyjson               JSON parser + writer.
//   - libcurl              HTTP (datasets, LLM bindings).
//   - utf8proc / simdutf   Unicode normalisation + validation.
//
// All deps are pinned by SHA-256 and built from source as part
// of libmochi.a. The driver never links a system copy of any
// substrate dep.
//
// Phase 0 ships only this doc.go. Phase 1 (hello world)
// introduces the first header (mochi/print.h) and the first
// source file (src/print.c).
package runtime
