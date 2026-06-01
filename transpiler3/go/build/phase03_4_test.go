package build

import (
	"testing"
)

// TestPhase3ListRecords is the MEP-54 Phase 3.4 gate for the Go transpiler.
//
// Phase 3.4 lowering contract (list-of-records):
//
//   - A list whose element type is a record T lowers to []T.
//
//   - list_record[i].field → list_record[i].Field (direct field access on
//     slice element; Go struct field names are title-cased by the lowerer).
//
//   - Mutating a record field in a list: list_record[i].field = v lowers
//     to list_record[i].Field = v (Go allows this for non-pointer slice
//     element types because each []T element is addressable).
//
//   - for r in list_record { body } → for _, r := range list_record { ... }
//     (r is a copy; field reads are safe; mutations in the loop body do not
//     affect the original slice — consistent with Mochi's value semantics).
func TestPhase3ListRecords(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"list_record_",
	)
}
