// Package corpus holds compiler3 hand-built IR programs used to
// validate end-to-end builds before the Mochi-source frontend lands on
// compiler3. Mirrors compiler2/corpus so the bench harness can drop in
// vm3 alongside vm2 without rewriting templates.
//
// Phase 0 ships only the scaffold; programs port over in Phase 2.
package corpus

import "mochi/compiler3/ir"

// Program names one corpus entry: a function builder keyed by name,
// parameterized by N (the per-program size knob shared with bench/).
type Program struct {
	Name  string
	Build func(n int64) *ir.Function
}

// All returns every registered corpus program. Phase 0 ships an empty
// list; Phase 2 starts populating it.
func All() []*Program { return nil }
