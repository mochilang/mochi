package colour

import "mochi/transpiler3/c/aotir"

// Colour represents the async/sync colour of a function in the call graph.
type Colour int

const (
	// Blue is a synchronous function.
	Blue Colour = iota
	// Red is an asynchronous function (must be awaited).
	Red
)

// ColourMap maps function names to their resolved Colour.
type ColourMap map[string]Colour

// Analyse performs the async colouring pass over prog.
// In Phase 0 all functions are coloured Blue (synchronous).
// Phase 6 (async/await) replaces this with a full fixed-point propagation.
func Analyse(prog *aotir.Program) ColourMap {
	cm := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		cm[fn.Name] = Blue
	}
	return cm
}
