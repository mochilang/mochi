// Package colour holds the async colouring infrastructure for the
// PHP target. Every function is always Blue (synchronous): Phase 11
// shipped async/await as a synchronous value wrapper (mochi_future_make
// just stores the value, mochi_future_await returns it) rather than
// the originally-planned Amphp/Fiber dispatch, so no PHP function
// ever needs an `Amp\Future<T>` return type and no call site ever
// needs to `await`.
//
// The pass and its result are still wired through build.Build for
// symmetry with the other transpiler3 targets (C, BEAM, JVM, .NET,
// Swift), and to keep a single place to flip the design back to
// real futures if a future MEP wants them.
package colour

import "mochi/transpiler3/c/aotir"

// Colour is a per-function tag controlling lowering. Currently only
// Blue is produced; Red is reserved for a future revival of real
// async dispatch.
type Colour int

const (
	// Blue marks a synchronous function. Its lowered PHP signature
	// returns `T` (or `void`) and is called directly.
	Blue Colour = iota
	// Red is reserved. The PHP target's Phase 11 design avoided real
	// futures, so this colour is never produced today. Keeping the
	// constant lets a future async revival plug in without breaking
	// the public Colour type.
	Red
)

// ColourMap maps function names to their colour.
type ColourMap map[string]Colour

// Compute returns the colour assignment for prog. Every entry is
// Blue: the PHP target chose sync wrappers over fibers/Amp, so no
// function is ever Red. See package doc.
func Compute(prog *aotir.Program) ColourMap {
	m := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		m[fn.Name] = Blue
	}
	return m
}
