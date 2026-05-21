// vm3runner is the bench/crosslang subprocess entry for the
// compiler3 + runtime/vm3 + runtime/jit/vm3jit stack. It mirrors
// bench/vm2runner: take a program identifier + N, run it the canonical
// "repeat" count, and print {"duration_us": X, "output": Y}.
//
// Program names are accepted both as the bare compiler3/corpus slug
// (e.g. "nsieve") and as the crosslang "<cat>_<name>" shape
// (e.g. "bg_nsieve"). The latter is what bench/crosslang already
// passes to vm2runner, so a single -program flag works for both.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// repeats records the per-template repeat count baked into each
// bench/template/<cat>/<name> peer. Keys match the bare compiler3
// corpus slug so vm3 and vm2 columns share the same per-iter cost.
var repeats = map[string]int{
	"fact_rec":           1000,
	"fib_iter":           1000,
	"fib_rec":            1,
	"mul_loop":           1000,
	"prime_count":        100,
	"sum_loop":           1000,
	"strings_concat_loop": 1000,
	"lists_fill_sum":     1000,
	"maps_fill_sum":      1000,
	"nsieve":             50,
	"binary_trees":       1,
	"fannkuch_redux":     1,
	"mandelbrot":         1,
	"n_body":             1,
	"spectral_norm":      1,
	"reverse_complement": 1,
	"fasta":              1,
	"k_nucleotide":       1,
}

// stripCat strips a "<cat>_" prefix from a vm2runner-style program
// name and returns the bare compiler3 corpus slug. Only categories
// whose corpus slug omits the category are stripped: bg/math entries
// register as plain "nsieve" / "fact_rec", while the strings, lists,
// and maps subsystems carry the category in the slug itself
// ("strings_concat_loop", "lists_fill_sum", "maps_fill_sum") so the
// prefix must be preserved.
func stripCat(name string) string {
	for _, prefix := range []string{"math_", "bg_"} {
		if rest, ok := strings.CutPrefix(name, prefix); ok {
			return rest
		}
	}
	return name
}

func main() {
	prog := flag.String("program", "", "corpus program slug (e.g. nsieve or bg_nsieve)")
	n := flag.Int64("n", 0, "size parameter")
	flag.Parse()

	if *prog == "" {
		die("vm3runner: -program is required")
	}
	slug := stripCat(*prog)
	repeat, ok := repeats[slug]
	if !ok {
		die("vm3runner: unknown program %q", *prog)
	}

	var p *corpus.Program
	for _, c := range corpus.All() {
		if c.Name == slug {
			p = c
			break
		}
	}
	if p == nil {
		die("vm3runner: program %q not in compiler3/corpus.All()", slug)
	}

	// Build once, JIT once. The timed region is just the inner repeat
	// loop, matching every other peer.
	program := p.Build(*n)
	cfs := vm3jit.CompileProgram(program)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()

	vm := vm3.NewWithProgram(program)
	fn := program.Funcs[program.Entry]
	args := []int64{*n}

	var last int64
	start := time.Now()
	for range repeat {
		got, err := vm.RunWithArgs(fn, args)
		if err != nil {
			die("vm3runner: run %s: %v", slug, err)
		}
		last = got.Int()
	}
	durUs := float64(time.Since(start).Microseconds())

	if err := json.NewEncoder(os.Stdout).Encode(map[string]any{
		"duration_us": durUs,
		"output":      last,
	}); err != nil {
		die("vm3runner: encode: %v", err)
	}
}

func die(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}
