// Package colour computes the async/sync colour of each function in the call graph.
package colour

import "mochi/transpiler3/c/aotir"

// Colour represents the async/sync colour of a function.
type Colour int

const (
	Blue Colour = iota // synchronous
	Red                // async
)

// ColourMap maps function names to their resolved Colour.
type ColourMap map[string]Colour

// Analyse colours all functions Blue (sync) in Phase 1.
// Phase 11 replaces this with real async propagation.
func Analyse(prog *aotir.Program) ColourMap {
	cm := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		cm[fn.Name] = Blue
	}
	return cm
}
