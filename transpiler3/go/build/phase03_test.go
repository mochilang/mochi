package build

import (
	"testing"
)

// TestPhase3Lists is the MEP-54 Phase 3.1 gate for the Go transpiler.
//
// Phase 3.1 lowering contract:
//
//   - List literals → Go slice literals: []T{e1, e2, ...}
//
//   - list[i] → list[i] (direct index; bounds check panics via Go runtime).
//
//   - append(list, v) → append(list, v).
//
//   - len(list) → len(list) (int64 cast: int64(len(list))).
//
//   - sort(list) → a cloned sorted slice using sort.Slice (ascending).
//     Float lists use sort.Slice with < comparison.
//
//   - list contains v → mochiListContains(list, v) helper (linear scan).
//
//   - for x in list { body } → for _, x := range list { body }.
//
//   - max/min on lists → mochiListMax / mochiListMin helpers.
func TestPhase3Lists(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"list_",
	)
}
