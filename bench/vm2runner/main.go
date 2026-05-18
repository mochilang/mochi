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
	"mochi/runtime/jit/vm2jit"
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
	// Strings subsystem (MEP-24 §2). The repeat count keeps wall-clock
	// in the same ~1ms ballpark as the math kernels so the harness sees
	// it as just another column.
	"strings_concat_loop": 1000,
	// Lists subsystem (MEP-24 §3).
	"lists_fill_sum": 1000,
	// Maps subsystem (MEP-24 §4).
	"maps_fill_sum": 1000,
	// MEP-23 Benchmarks Game suite. Lower repeat counts because each
	// invocation already runs the full BG kernel once at the chosen N.
	"bg_nsieve":       50,
	"bg_binary_trees": 1,
	// MEP-39: fannkuch_redux scaled form. N is the outer trial count
	// (the inner permutation size is fixed at 7), so a single Build
	// call already encodes N iterations. The harness loops once.
	"bg_fannkuch_redux": 1,
	// MEP-39 §6.2: mandelbrot. N is the side of the N x N grid; a
	// single Build call already encodes N^2 pixel iterations.
	"bg_mandelbrot": 1,
	// MEP-39 §6.3: n_body. N is the outer step count; a single Build
	// call already runs N advance + posUpdate iterations.
	"bg_n_body": 1,
	// MEP-39 §6.4: spectral_norm. N is the vector dimension; the
	// inner power-method always runs 10 iterations regardless of N.
	"bg_spectral_norm": 1,
	// MEP-39 §6.5: reverse_complement. N is the buffer length; one
	// fill + one reverse-complement pass + one sum at that size.
	"bg_reverse_complement": 1,
	// MEP-39 §6.6: fasta. N is the LCG iteration count; one LCG step
	// + one cumprob lookup + one hash update per inner iter.
	"bg_fasta": 1,
	// MEP-39 §6.7: k_nucleotide. N is the LCG iteration count; one
	// LCG step + 1-mer inc + 2-mer inc per inner iter, then a 20-key
	// summarise pass at the end.
	"bg_k_nucleotide": 1,
	// MEP-39 §6.8: pidigits. N is the decimal-digit count emitted by
	// the Gibbons unbounded spigot; one Build call already encodes the
	// full produce/consume loop until N digits are folded into the
	// rolling i64 hash.
	"bg_pidigits": 1,
	// MEP-39 §6.9: regex_redux. N is the DNA stream length; one LCG
	// step + one window shift + one match check per inner iter.
	"bg_regex_redux": 1,
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

	// MEP-39 §6.15: best-effort JIT pass before the timed loop.
	// CompileProgram silently skips functions the backend can't handle
	// (unsupported arch, NumRegs over cap, unprofitable, unlowerable
	// opcode), leaving fn.JITCode nil so the interpreter's OpCall fast
	// path naturally falls through. Handles are kept alive for the
	// duration of main; the OS reclaims pages on exit.
	jitHandles, _, _ := vm2jit.CompileProgram(program)
	_ = jitHandles

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
