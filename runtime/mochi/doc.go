// Package mochi is the Mochi standard runtime for the Go target.
//
// Each sub-package exposes one piece of Mochi semantics as idiomatic
// Go. The packages have normal Go tests, ordinary example functions,
// and a Go user can import any of them directly: nothing here depends
// on the Mochi VM, the compiler, or the parser. MEP-43 §3.3 specifies
// the contract: the Go-target emitter calls into these packages
// instead of inlining its own arithmetic, instead of re-implementing
// query algebra per call, and instead of templating strings around
// printf-style formats.
//
// The layout is deliberately flat. The packages do not import each
// other (apart from query, which composes lists+maps+sets), so a Go
// user paying for `runtime/mochi/strings` does not transitively pull
// in JSON, YAML, decimal, or query.
//
// Status: MEP-43 Phase 3 lands the package skeleton, the operations
// the existing Mochi builtins cover today (len, upper, lower, reverse,
// contains, indexOf, split, join, replace, append, keys, values,
// avg/sum/min/max, json, yaml, fmt.Print, decimal arithmetic, time),
// and the query algebra (Filter, Map, Sort, GroupBy, Join, LeftJoin,
// OuterJoin, Limit, Take). The emitter wiring lands in Phase 4 and
// Phase 5 (see MEP-43 §10).
package mochi
