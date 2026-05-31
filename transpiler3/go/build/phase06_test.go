package build

import (
	"testing"
)

// TestPhase6Closures is the MEP-54 Phase 6 gate for the Go transpiler.
//
// Phase 6 lowering contract:
//
//   - Anonymous fun literals → Go anonymous func literals.
//     `fun(x: int): int => x * 2` → `func(x int64) int64 { return x * 2 }`.
//
//   - Capturing closures: the aotir closure-conversion pass has already
//     extracted capture slots into an env struct. The Go lowerer re-inlines
//     them as plain captured variables in the anonymous func (Go closures
//     capture by reference; the lowerer emits a local copy at the closure
//     definition site for each slot to match Mochi's by-value capture semantics).
//
//   - Fun-typed variables: `let f = fun(x: int): int => x + 1` → `f := func(x int64) int64 { return x + 1 }`.
//
//   - Passing functions as arguments: direct Go function value passing.
//
//   - Higher-order builtins:
//       map(xs, fn)          → mochiListMap(xs, fn)
//       filter(xs, fn)       → mochiListFilter(xs, fn)
//       reduce(xs, fn, init) → mochiListFoldl(xs, fn, init)
//     Each helper is a generic Go function emitted in the program's prelude.
func TestPhase6Closures(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"closure_",
		"lambda_",
		"hof_",
	)
}
