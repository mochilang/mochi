package build

import (
	"testing"
)

// TestPhase3Maps is the MEP-54 Phase 3.2 gate for the Go transpiler.
//
// Phase 3.2 lowering contract:
//
//   - Map literals → map[K]V{k1: v1, ...}.
//
//   - map_get(m, k) → m[k] (zero value if absent; consistent with vm3).
//
//   - map_has(m, k) → mochiMapHas(m, k) helper that returns the bool
//     from the two-valued Go index expression.
//
//   - map_put(m, k, v) → m[k] = v statement.
//
//   - map_keys(m) → mochiMapKeys(m) returning []K sorted ascending.
//
//   - map_values(m) → mochiMapValues(m) returning []V in sorted-key order.
//
//   - len(map) → int64(len(m)).
func TestPhase3Maps(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"map_",
	)
}
