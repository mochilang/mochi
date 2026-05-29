---
title: "03. Prior-art transpilers"
sidebar_position: 4
sidebar_label: "03. Prior art"
description: "Survey of source-to-Go transpilers and code generators, plus the alternative-language compilers that target Go as a backend, plus the rejected approaches."
---

# 03. Prior-art transpilers

This note surveys what other source-to-Go projects exist, what they got right, what they got wrong, and how MEP-54 differs.

## Source-to-Go transpilers and generators

### GopherJS

GopherJS (Richard Musiol, 2013-present) compiles Go source to JavaScript. The reverse direction of what we want, but the design lessons transfer: GopherJS uses a `Generator` struct that walks the Go AST and emits a JS string. Whitespace bugs were a recurring issue in early GopherJS releases until the renderer was rewritten to use a structural intermediate. We start with the structural intermediate (`gotree`) to skip that learning cycle.

### TinyGo

TinyGo (2018-present) is a separate Go compiler targeting microcontrollers and wasm. It is **not** a transpiler (it consumes Go AST and emits LLVM IR), but it informs our wasm strategy: TinyGo's `tinygo build -target=wasi` was the dominant pre-1.21 path for Go-to-WASI; once `GOOS=wasip1` shipped in Go 1.21, the standard toolchain became viable for wasi targets without TinyGo's runtime restrictions (no full reflect, limited concurrency).

MEP-54 defaults to standard `go build` for all targets including wasm. TinyGo is a Phase 17 sub-target for users who need bare-metal wasm without the Go runtime, but is not the default.

### Joy

Joy (Matt Mueller, 2016-2018, dormant) was a Go-to-JS transpiler with a structural AST design similar to what we use. Joy proved out the "render via canonical formatter" pattern (it piped through Babel for JS formatting); we adapt the pattern by piping through `go/format` for Go formatting.

### kube-openapi, sqlc, stringer, mockgen, protoc-gen-go

These are not transpilers but code generators that produce Go source from a non-Go input (OpenAPI specs, SQL schemas, Go interface types, protobuf definitions). Every one of them uses some variant of `text/template` plus a post-pass through `gofmt`. The whitespace-bug class is recurring across the entire space; kubebuilder's "the import block is malformed" failure mode is a known wart.

We sidestep this by constructing a structural AST that cannot produce malformed import blocks (the `gotree.ImportDecl` node lists imports as `[]string`, not as raw lines).

### gccgo, gollvm

Alternative Go compilers (not transpilers). They inform the toolchain matrix: in principle we could gate against gccgo for ABI conformance, but in practice gccgo lags the gc toolchain by 1-2 years on language features and is unmaintained for many Linux distros. We do not include gccgo in the production gate.

## Alternative-language compilers targeting Go

### esc.go (deprecated), go-bindata (deprecated)

Asset embedders that emit `.go` files containing `[]byte` literals. These prove out the "drop a `.go` file into a Go module and let `go build` pick it up" pattern. MEP-54 uses the same pattern for the emitted `main.go`.

### sql-migrate, atlas

Database migration tools that emit Go source describing schema changes. Not directly relevant to our case but they validate the "Go as a config / artefact language" use case.

### wasm-bindgen-equivalents

There is no wasm-bindgen analogue for Go on the browser side; the Go runtime ships `wasm_exec.js` as the glue layer. Phase 17 includes `wasm_exec.js` in the output directory of `TargetGoWasmJS`.

## Comparison table

| Project | Direction | AST style | Formatter pass | Status |
|---------|-----------|-----------|----------------|--------|
| GopherJS | Go → JS | structural (after rewrite) | JS-side prettier | active |
| TinyGo | Go AST → LLVM IR | n/a (consumes AST) | n/a | active |
| Joy | Go → JS | structural | Babel | dormant |
| kube-openapi | OpenAPI → Go | text/template | gofmt | active |
| sqlc | SQL → Go | text/template | gofmt | active |
| stringer | Go interface → Go | text/template | gofmt | active |
| protoc-gen-go | proto → Go | structural (`google.golang.org/protobuf/compiler/protogen`) | gofmt | active |
| **MEP-54** | **Mochi → Go** | **structural (gotree)** | **go/format.Source** | **active** |

The structural-AST projects (GopherJS rewrite, Joy, protoc-gen-go) have meaningfully lower whitespace-bug rates than the text/template projects. We follow the structural-AST pattern.

## Specific decisions informed by prior art

1. **Renderer through `go/format.Source`, not `go/printer`.** kube-openapi initially used `go/printer.Fprint` and hit a class of leading-comment placement bugs; the rewrite to `format.Source` (which re-parses and re-prints) fixed them. We start with `format.Source` to skip the bug class.

2. **Import block as `[]string`, not `[]ImportSpec`.** Protocol Buffers' `protogen.GoIdent` model is a richer pattern but more code to maintain. For our single-package emit case, the simple list of `import "..."` lines is enough; `goimports`-equivalent group-and-sort behaviour is delegated to `go/format.Source`.

3. **No alias-import support in the gotree node.** Aliases are rare in lowered code (the only common case is `import _ "embed"` for the directive form, which we render explicitly). Not having them in the model keeps the node simple.

4. **One file per emit, not multi-file.** Multi-file emit (one file per top-level declaration, say) would match Go style for hand-written code but produces inscrutable per-fixture diffs in the gold-file testing flow. Single-file `main.go` is what `go run` expects and matches the `expect.txt`-based test flow.

5. **Vendor mode by default.** kube-openapi and sqlc both default to proxy mode (requires internet), which has bitten reproducibility-sensitive users. We vendor by default and offer proxy mode as an opt-in.

## What MEP-54 does not borrow

- **gRPC / protobuf code generation patterns.** Mochi types do not have a wire format; the lowering is direct-to-Go without an IDL intermediate.
- **`go generate` directives.** The Mochi build is driven by `mochi build`, not by `go generate`. Users who want to integrate Mochi into a `go generate` flow can shell out to `mochi build` from a `//go:generate` directive in a host Go file, but the integration is out of scope for the MEP.
- **Go module mirroring.** We rely on the public `proxy.golang.org` for the runtime module's distribution; we do not run our own mirror.
