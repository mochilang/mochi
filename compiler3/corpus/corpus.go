package corpus

import "mochi/runtime/vm3"

// Program names one corpus entry: a function builder keyed by name,
// parameterized by N (the per-program size knob shared with bench/).
type Program struct {
	Name string
	// Build constructs a single-function Program from N. The function
	// takes N as its first i64 parameter (regsI64[0]) and returns an
	// i64 result. Programs that recurse return a multi-function Program
	// with Entry pointing at the public function.
	Build func(n int64) *vm3.Program
}

// All returns every registered corpus program.
func All() []*Program {
	return []*Program{
		FibIter,
		SumLoop,
		MulLoop,
		FactRec,
		FibRec,
		PrimeCount,
	}
}
