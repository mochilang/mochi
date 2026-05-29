// Package pysrc is the Go-side surrogate for CPython's ast module.
//
// MEP-51 Phase 1 ships a deterministic direct serialiser to .py source text:
// every node implements PyString(indent int) string and the package writes
// Python 3.12+ output ready to be passed through ruff format + ruff check.
//
// The node set grows phase by phase; the ast.unparse subprocess shell-out
// described in the MEP-51 spec is deferred to Phase 16 (reproducibility),
// when the renderer is replaced by a one-shot subprocess to libcst-pinned
// CPython. For Phase 1, direct Go-side rendering keeps the gate boot-strappable
// without a CPython dependency in the build pipeline.
package pysrc
