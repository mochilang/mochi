// Package colour assigns each aotir function a Blue (sync) or Red
// (async) colour. Phase 1 ships a trivial stub: every function is
// Blue, because the hello-world fixture set contains no agents,
// fetch, sleep, llm.generate, or any other awaitable site.
//
// Phase 9 (agents) seeds the first Red colour; Phase 11 (async
// pass) implements the full call-graph fixed-point per MEP-52 §5.
// Until then, Compute returns a map where every function is Blue.
package colour

import "mochi/transpiler3/c/aotir"

// Colour is the binary colour assigned to a function.
type Colour int

const (
	// Blue means the function body never awaits; it lowers to a
	// synchronous TS `function` (no `async` modifier).
	Blue Colour = iota
	// Red means the function body contains at least one await
	// site (transitively); it lowers to `async function` and the
	// return type wraps in `Promise<...>`.
	Red
)

// ColourMap maps a function (by aotir.Function.Name) to its
// computed colour. The TS lower pass consults this when deciding
// the function header shape and the call-site await form.
type ColourMap map[string]Colour

// Compute returns a ColourMap for prog. Phase 1: every function
// is Blue. The signature matches Phase 11's full implementation
// so the call sites in the lower pass do not need to change as
// the colour set grows.
func Compute(prog *aotir.Program) ColourMap {
	if prog == nil {
		return ColourMap{}
	}
	out := make(ColourMap, len(prog.Functions))
	for _, fn := range prog.Functions {
		out[fn.Name] = Blue
	}
	return out
}
