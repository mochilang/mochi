package build

import (
	"testing"
)

// TestPhase3Sets is the MEP-54 Phase 3.3 gate for the Go transpiler.
//
// Phase 3.3 lowering contract:
//
//   - Set literals → map[T]struct{}{v1: {}, v2: {}, ...}.
//
//   - set_add(s, v) → s[v] = struct{}{}.
//
//   - set_has(s, v) → mochiSetHas(s, v) using the two-valued map index.
//
//   - len(set) → int64(len(s)).
//
//   - for x in set { body } → for x := range s { body } (iteration order
//     is deterministic only if the fixture does not rely on ordering; vm3
//     and the Go backend both produce consistent sorted output for the
//     test corpus because fixtures print via sorted keys).
//
//   - set_dedup(list) → mochiSetDedup helper that inserts each element into
//     a map, then returns the sorted keys as a new list (deduplicated).
func TestPhase3Sets(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"set_",
	)
}
