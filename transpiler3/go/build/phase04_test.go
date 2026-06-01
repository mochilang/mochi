package build

import (
	"testing"
)

// TestPhase4Records is the MEP-54 Phase 4 gate for the Go transpiler.
//
// Phase 4 lowering contract:
//
//   - Record type declarations → Go struct with exported fields.
//     `type Pt { x: int  y: int }` → `type Pt struct { X int64; Y int64 }`.
//
//   - Record literals → struct literals: `Pt { x: 1, y: 2 }` → `Pt{X: 1, Y: 2}`.
//
//   - Field access: `p.x` → `p.X`.
//
//   - Record equality: `a == b` where a, b are of record type lowers to
//     `mochiEq_Pt(a, b)`, a generated function comparing each field.
//     For nested list/map fields the comparison delegates to reflect.DeepEqual.
//
//   - Record inequality: `a != b` → `!mochiEq_Pt(a, b)`.
//
//   - Multiple record types in one program: one mochiEq_ per type.
//
//   - Record variable assignment: `let p = Pt{...}` → `p := Pt{...}`.
//     Reassignment: `p = Pt{...}` → `p = Pt{...}` (value semantics, no pointer).
func TestPhase4Records(t *testing.T) {
	runFixturesMatchingPrefixes(t,
		"record_",
		"join_inner_", "join_cross_", "join_left_",
	)
}
