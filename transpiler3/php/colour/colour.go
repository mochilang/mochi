// Package colour computes the async colouring of an aotir
// program for the PHP target.
//
// PHP 8.1+ fibers provide stackful coroutines; combined with
// Amphp v3 (amphp/amp, amphp/pipeline) we expose
// `Amp\Future<T>` as the return type of async functions and
// drive them with `Amp\async(...)`. Colouring decides which
// Mochi functions are async (their lowered PHP signature
// returns `\Amp\Future<T>` instead of `T`).
//
// Phase 0 ships a stub colour map: every function is `Blue`
// (synchronous). The full forward-and-backward dataflow lands
// in Phase 11 when async/await semantics first appear.
package colour

import "mochi/transpiler3/c/aotir"

// Colour is a per-function tag controlling lowering.
type Colour int

const (
	// Blue marks a synchronous function. Its lowered PHP signature
	// returns `T` (or `void`) and is called directly.
	Blue Colour = iota
	// Red marks an asynchronous function. Its lowered PHP signature
	// returns `\Amp\Future<T>` and call sites lower `await f(...)`
	// to `f(...)->await()`.
	Red
)

// ColourMap maps function names to their colour.
type ColourMap map[string]Colour

// Compute returns the colour assignment for prog. Phase 0
// returns all-Blue; Phase 11 replaces this with the real
// async/await fixed-point pass.
func Compute(prog *aotir.Program) ColourMap {
	m := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		m[fn.Name] = Blue
	}
	return m
}
