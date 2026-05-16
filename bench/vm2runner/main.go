// vm2runner is the bench/runner.go subprocess entry for the compiler2
// + runtime/vm2 stack. It mirrors what every other language baseline
// does: take a parameterized program identifier + N, run it the
// canonical "repeat" count, and print {"duration_us": X, "output": Y}.
//
// The runner reuses the IR builders in compiler2/corpus so the program
// shape is byte-identical to what runtime/vm2/bench/corpus_test.go
// runs. That means a regression in either harness shows up in both.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"time"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// repeats records the per-template repeat count baked into each
// .mochi benchmark template. Keep these in sync with bench/template/.
var repeats = map[string]int{
	"math_fact_rec":    1000,
	"math_fib_iter":    1000,
	"math_fib_rec":     1,
	"math_mul_loop":    1000,
	"math_prime_count": 100,
	"math_sum_loop":    1000,
	// MEP-17 in-process corpus runs once per invocation; the harness
	// loops externally via b.N.
	"fib":      1,
	"iter_sum": 1,
}

func main() {
	prog := flag.String("program", "", "corpus program name (e.g. math_fact_rec)")
	n := flag.Int64("n", 0, "size parameter")
	flag.Parse()

	if *prog == "" {
		die("vm2runner: -program is required")
	}
	repeat, ok := repeats[*prog]
	if !ok {
		die("vm2runner: unknown program %q", *prog)
	}

	var builder func(int64) *corpus.Program
	for _, p := range corpus.All() {
		if p.Name == *prog {
			pp := p
			builder = func(int64) *corpus.Program { return &pp }
			break
		}
	}
	if builder == nil {
		die("vm2runner: program %q not in corpus.All()", *prog)
	}
	p := builder(*n)

	// Compile once outside the timed region: this matches every other
	// language harness, which times only the inner repeat loop.
	m := p.Build(*n)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	program, err := emit.Compile(m)
	if err != nil {
		die("vm2runner: emit %s: %v", *prog, err)
	}

	// Single VM reused across reps. Matches Lua / CPython, where the
	// interpreter state is created once and the user code runs in a
	// loop; otherwise we'd be benchmarking VM construction overhead
	// per rep, not the interpreter loop. Run() resets Stack/Frames
	// internally between invocations.
	vm := vm2.New(program)
	var last int64
	start := time.Now()
	for i := 0; i < repeat; i++ {
		got, err := vm.Run()
		if err != nil {
			die("vm2runner: run %s: %v", *prog, err)
		}
		last = got.Int()
	}
	durUs := float64(time.Since(start).Microseconds())

	if err := json.NewEncoder(os.Stdout).Encode(map[string]any{
		"duration_us": durUs,
		"output":      last,
	}); err != nil {
		die("vm2runner: encode: %v", err)
	}
}

func die(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}
