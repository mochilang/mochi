// Package colour assigns a sync/async colour to every aotir function.
// Phase 0 ships the trivial all-green analyser; Phase 11 introduces
// the genuine effect-flow analysis (await colours an async fn red,
// and call-graph propagation paints every caller).
package colour

import "mochi/transpiler3/c/aotir"

// Colour is the effect colour of a function.
type Colour int

const (
	// Green is a sync (non-async) function.
	Green Colour = iota
	// Red is an async function.
	Red
)

// ColourMap maps mangled function name to its Colour.
type ColourMap map[string]Colour

// Analyse returns a ColourMap for prog. Phase 0 paints everything Green;
// Phase 11 replaces this with effect-flow analysis.
func Analyse(prog *aotir.Program) ColourMap {
	m := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		m[fn.Name] = Green
	}
	return m
}
