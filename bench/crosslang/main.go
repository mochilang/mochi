// crosslang runs the six MEP-23 baseline programs (math/fact_rec,
// math/fib_iter, math/fib_rec, math/mul_loop, math/prime_count,
// math/sum_loop) head-to-head across runtime/vm2, CPython, and Lua,
// then prints a markdown table that drops into MEP-23.
//
// It deliberately bypasses bench/runner.go to avoid the broken
// transpiler imports that block cmd/mochi-bench from building on
// current main. The transpiled rows (mochi_go, mochi_py, mochi_c,
// mochi_ts) are not what this sweep is about; this sweep is the
// vm2-as-runtime-vs-other-interpreters comparison.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"syscall"
	"time"
)

type program struct {
	category string // bench/template/<category>/<name> subdir; "" defaults to "math"
	name     string // bench/template/<category>/<name> subdir
	ns       []int  // sizes to run
}

// cat returns the effective category, defaulting math entries to "math"
// so existing rows do not need to be edited when new categories land.
func (p program) cat() string {
	if p.category == "" {
		return "math"
	}
	return p.category
}

// Per-program sizes. fact_rec and mul_loop cap at sizes whose result
// fits in vm2's 48-bit signed Cell payload (max ~1.4e14): 16! and 16!
// respectively. Lifting that cap requires vm2 boxed-int support.
//
// The largest N from each program was dropped (per user feedback): the
// big-N rows ran for tens of seconds without changing the relative
// ranking of vm2 vs CPython vs Lua. The remaining sizes already span
// 10x–100x dynamic range, which is plenty for a comparison sweep.
var programs = []program{
	{name: "fact_rec", ns: []int{10, 13}},
	{name: "fib_iter", ns: []int{10, 20}},
	{name: "fib_rec", ns: []int{15, 20}},
	{name: "mul_loop", ns: []int{10, 13}},
	{name: "prime_count", ns: []int{50, 100}},
	{name: "sum_loop", ns: []int{1000, 10000}},
	// MEP-24 §2 strings subsystem. Concat loop exercises the
	// allocating string-concat path: N appends per inner call,
	// repeated `repeats` times by each language harness.
	{category: "strings", name: "concat_loop", ns: []int{10, 30}},
	// MEP-24 §3 lists subsystem. Fill+sum exercises the OpNewList /
	// OpListPush growth path plus OpListGet on the read-back pass.
	{category: "lists", name: "fill_sum", ns: []int{10, 100}},
	// MEP-24 §4 maps subsystem. Same fill+sum shape but keyed by int;
	// exercises OpNewMap, OpMapSet, OpMapGet through Go's map[any]Cell.
	{category: "maps", name: "fill_sum", ns: []int{10, 100}},
	// MEP-23 Benchmarks Game suite. nsieve exercises the list payload
	// throughput path; binary_trees exercises container construction
	// and reclamation (MEP-36 headline). N for binary_trees is the
	// depth: 2^depth trees built and discarded, each with 2^(depth+1)-1
	// nodes, so the allocation count grows fast — keep depth modest.
	{category: "bg", name: "nsieve", ns: []int{1000, 10000}},
	{category: "bg", name: "binary_trees", ns: []int{8, 10}},
	// MEP-39: BG fannkuch_redux. N is the outer trial count over a fixed
	// 7-element inner permutation. Output is the sum of per-trial flip
	// counts. See bench/template/bg/fannkuch_redux/ for the Mochi peer.
	{category: "bg", name: "fannkuch_redux", ns: []int{1000, 10000}},
	// MEP-39 §6.2: BG mandelbrot. N is the side of an N x N grid in
	// [-2, 1] x [-1, 1] with maxIter = 50; output is the sum of
	// per-pixel escape counts. See bench/template/bg/mandelbrot/.
	{category: "bg", name: "mandelbrot", ns: []int{100, 200}},
	// MEP-39 §6.3: BG n_body. N is the number of advance + posUpdate
	// step iterations over the canonical five-body Sun+gas-giants
	// configuration. Output is int64(energy * 1e9), so cross-lang
	// peers can integer-compare without f64 stringification quirks.
	// See bench/template/bg/n_body/.
	{category: "bg", name: "n_body", ns: []int{1000, 5000}},
	// MEP-39 §6.4: BG spectral_norm. N is the dimension of the vector
	// fed to the power-method estimate of the dominant eigenvalue of
	// the Hilbert-like matrix A(i, j) = 1/((i+j)(i+j+1)/2 + i + 1).
	// Output is floor(sqrt(uBu/uu) * 1e9). The inner power loop always
	// runs 10 iterations (5 pairs of AtAu) regardless of N. See
	// bench/template/bg/spectral_norm/.
	{category: "bg", name: "spectral_norm", ns: []int{100, 200}},
	// MEP-39 §6.5: BG reverse_complement. N is the buffer length;
	// the buffer is filled with the ACGT cycle, the entire range
	// is reverse-complemented into a second buffer, and the peers
	// integer-compare sum(out). When N is a multiple of 4, the
	// expected output is (N/4) * 287. See bench/template/bg/reverse_complement/.
	{category: "bg", name: "reverse_complement", ns: []int{4096, 16384}},
}

type result struct {
	Program     string  `json:"program"`
	N           int     `json:"n"`
	Lang        string  `json:"lang"`
	DurationUs  float64 `json:"duration_us"`
	MemoryBytes int64   `json:"memory_bytes"`
	Output      any     `json:"output"`
	Err         string  `json:"err,omitempty"`
}

// aggregate is the Benchmarks Game-style summary of repeat invocations of
// one (program, n, lang) tuple. Median is the headline; min/max bound the
// noise floor. See MEP-23 §"Benchmarks Game methodology" for the rules.
type aggregate struct {
	Program     string  `json:"program"`
	N           int     `json:"n"`
	Lang        string  `json:"lang"`
	Runs        int     `json:"runs"`
	MedianUs    float64 `json:"median_us"`
	MinUs       float64 `json:"min_us"`
	MaxUs       float64 `json:"max_us"`
	MemoryBytes int64   `json:"memory_bytes"` // max across runs
	Output      any     `json:"output"`       // from the median run
	Err         string  `json:"err,omitempty"`
}

func main() {
	outJSON := flag.String("json", "", "write per-result JSON array to this file")
	outMD := flag.String("md", "", "write markdown summary to this file")
	repeat := flag.Int("repeat", 1, "run each (program,n,lang) tuple this many times and report median (Benchmarks Game-style)")
	langs := flag.String("langs", "vm2,py,pypy,lua,luajit,go", "comma-separated subset of languages to run")
	progFilter := flag.String("programs", "", "optional comma-separated <cat>/<name> filter (e.g. bg/fannkuch_redux)")
	flag.Parse()

	wantLang := func(l string) bool {
		for _, s := range strings.Split(*langs, ",") {
			if strings.TrimSpace(s) == l {
				return true
			}
		}
		return false
	}
	wantProg := func(cat, name string) bool {
		if *progFilter == "" {
			return true
		}
		key := cat + "/" + name
		for _, s := range strings.Split(*progFilter, ",") {
			if strings.TrimSpace(s) == key {
				return true
			}
		}
		return false
	}

	repoRoot, err := os.Getwd()
	if err != nil {
		die("getwd: %v", err)
	}
	tmp, err := os.MkdirTemp("", "mochi-crosslang-*")
	if err != nil {
		die("mktemp: %v", err)
	}
	defer os.RemoveAll(tmp)

	// Pre-build vm2runner once so each invocation is just the
	// interpreter doing the work, not the Go toolchain.
	vm2Bin := filepath.Join(tmp, "vm2runner")
	build := exec.Command("go", "build", "-o", vm2Bin, "./bench/vm2runner")
	build.Dir = repoRoot
	build.Stdout, build.Stderr = os.Stdout, os.Stderr
	if err := build.Run(); err != nil {
		die("vm2runner build: %v", err)
	}

	if *repeat < 1 {
		*repeat = 1
	}

	var aggs []aggregate
	for _, p := range programs {
		if !wantProg(p.cat(), p.name) {
			continue
		}
		for _, n := range p.ns {
			if wantLang("vm2") {
				aggs = append(aggs, measure(*repeat, func() result { return runVM2(vm2Bin, p.cat(), p.name, n) }))
			}
			if wantLang("py") {
				aggs = append(aggs, measure(*repeat, func() result { return runNative(repoRoot, tmp, p.cat(), p.name, n, "py", "py", "python3") }))
			}
			if wantLang("pypy") {
				// PyPy runs the same .py source as CPython; the column
				// captures the JIT delta independently. macOS install:
				// `brew install pypy3.11` provides `pypy3.11` on PATH.
				// Linux: download from pypy.org, symlink as `pypy3`.
				aggs = append(aggs, measure(*repeat, func() result {
					return runNative(repoRoot, tmp, p.cat(), p.name, n, "py", "pypy", pypyBinary())
				}))
			}
			if wantLang("lua") {
				aggs = append(aggs, measure(*repeat, func() result { return runNative(repoRoot, tmp, p.cat(), p.name, n, "lua", "lua", "lua") }))
			}
			if wantLang("luajit") {
				// LuaJIT runs the same .lua source as the lua peer
				// because BG kernels use only Lua 5.1-compatible
				// features (no integer division, no bitwise ops in
				// the kernel itself; arithmetic is plain numbers).
				aggs = append(aggs, measure(*repeat, func() result { return runNative(repoRoot, tmp, p.cat(), p.name, n, "lua", "luajit", "luajit") }))
			}
			if wantLang("go") {
				aggs = append(aggs, measure(*repeat, func() result { return runGo(repoRoot, tmp, p.cat(), p.name, n) }))
			}
		}
	}

	for _, a := range aggs {
		if a.Err != "" {
			fmt.Printf("[%s n=%d] %-6s ERR: %s\n", a.Program, a.N, a.Lang, a.Err)
			continue
		}
		fmt.Printf("[%s n=%d] %-6s median=%10.0f us  min=%10.0f us  max=%10.0f us  mem=%8d B  -> %v\n",
			a.Program, a.N, a.Lang, a.MedianUs, a.MinUs, a.MaxUs, a.MemoryBytes, a.Output)
	}

	if *outJSON != "" {
		f, err := os.Create(*outJSON)
		if err != nil {
			die("create json: %v", err)
		}
		enc := json.NewEncoder(f)
		enc.SetIndent("", "  ")
		if err := enc.Encode(aggs); err != nil {
			die("encode json: %v", err)
		}
		f.Close()
	}

	md := renderMarkdown(aggs)
	if *outMD != "" {
		if err := os.WriteFile(*outMD, []byte(md), 0644); err != nil {
			die("write md: %v", err)
		}
	} else {
		fmt.Print("\n")
		fmt.Print(md)
	}
}

func runVM2(bin, cat, prog string, n int) result {
	progName := cat + "_" + prog
	cmd := exec.Command(bin, "-program", progName, "-n", fmt.Sprintf("%d", n))
	return runCmd(cat, prog, n, "vm2", cmd)
}

// runGo compiles the Go peer once per (program,n) tuple and then runs
// the binary. Pre-building isolates `go build` time from the timing
// window, matching what we do for vm2runner. Subsequent runs of the
// same (program,n) re-use the cached binary so the median-of-K loop
// pays the build cost exactly once.
//
// Source layout convention: `bench/template/<cat>/<prog>/<prog>.go`
// is a complete `package main` that takes N via `{{ .N }}`
// substitution (same as the .py / .lua peers), times an inner loop,
// and writes `{"duration_us": X, "output": Y}` to stdout.
func runGo(repoRoot, tmp, cat, prog string, n int) result {
	r := result{Program: cat + "/" + prog, N: n, Lang: "go"}
	// .go.tmpl rather than .go: an unrendered template that contains
	// `{{ .N }}` is not valid Go, so the .go extension would make
	// gopls and `go build ./...` complain. The template extension
	// keeps the file out of the package graph until we render it.
	srcPath := filepath.Join(repoRoot, "bench", "template", cat, prog, prog+".go.tmpl")
	src, err := os.ReadFile(srcPath)
	if err != nil {
		r.Err = err.Error()
		return r
	}
	rendered := strings.ReplaceAll(string(src), "{{ .N }}", fmt.Sprintf("%d", n))
	srcDir := filepath.Join(tmp, fmt.Sprintf("%s_%s_%d_go", cat, prog, n))
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		r.Err = err.Error()
		return r
	}
	srcOut := filepath.Join(srcDir, "main.go")
	if err := os.WriteFile(srcOut, []byte(rendered), 0644); err != nil {
		r.Err = err.Error()
		return r
	}
	bin := filepath.Join(srcDir, "bench")
	if _, err := os.Stat(bin); err != nil {
		build := exec.Command("go", "build", "-o", bin, srcOut)
		build.Dir = repoRoot
		var berr strings.Builder
		build.Stderr = &berr
		if err := build.Run(); err != nil {
			r.Err = fmt.Sprintf("go build: %v: %s", err, strings.TrimSpace(berr.String()))
			return r
		}
	}
	cmd := exec.Command(bin)
	return runCmd(cat, prog, n, "go", cmd)
}

// runNative drives a single-file interpreted peer.
//
// suffix is the source-file extension (e.g. "py", "lua") which selects
// the template; langLabel is the column key in the report (e.g. "py",
// "pypy", "lua", "luajit"); interpreter is the binary to exec.
func runNative(repoRoot, tmp, cat, prog string, n int, suffix, langLabel, interpreter string) result {
	srcPath := filepath.Join(repoRoot, "bench", "template", cat, prog, prog+"."+suffix)
	src, err := os.ReadFile(srcPath)
	if err != nil {
		return result{Program: cat + "/" + prog, N: n, Lang: langLabel, Err: err.Error()}
	}
	rendered := strings.ReplaceAll(string(src), "{{ .N }}", fmt.Sprintf("%d", n))
	out := filepath.Join(tmp, fmt.Sprintf("%s_%s_%d_%s.%s", cat, prog, n, langLabel, suffix))
	if err := os.WriteFile(out, []byte(rendered), 0644); err != nil {
		return result{Program: cat + "/" + prog, N: n, Lang: langLabel, Err: err.Error()}
	}
	cmd := exec.Command(interpreter, out)
	return runCmd(cat, prog, n, langLabel, cmd)
}

func runCmd(cat, prog string, n int, lang string, cmd *exec.Cmd) result {
	r := result{Program: cat + "/" + prog, N: n, Lang: lang}
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	start := time.Now()
	err := cmd.Run()
	_ = time.Since(start)
	if cmd.ProcessState != nil {
		if ru, ok := cmd.ProcessState.SysUsage().(*syscall.Rusage); ok && ru != nil {
			r.MemoryBytes = maxrssBytes(int64(ru.Maxrss))
		}
	}
	if err != nil {
		r.Err = fmt.Sprintf("%v: %s", err, strings.TrimSpace(stderr.String()))
		return r
	}
	var parsed struct {
		DurationUs float64 `json:"duration_us"`
		Output     any     `json:"output"`
	}
	if jerr := json.Unmarshal([]byte(strings.TrimSpace(stdout.String())), &parsed); jerr != nil {
		r.Err = fmt.Sprintf("invalid json: %v: %q", jerr, stdout.String())
		return r
	}
	r.DurationUs = parsed.DurationUs
	r.Output = parsed.Output
	return r
}

// maxrssBytes normalizes getrusage maxrss across platforms.
func maxrssBytes(raw int64) int64 {
	if runtime.GOOS == "darwin" {
		return raw // bytes on macOS
	}
	return raw * 1024 // kilobytes on Linux/BSD
}

// measure runs fn `runs` times and aggregates the results into one row.
// We report the median because (1) it's robust to outliers from OS
// scheduling jitter and (2) the Benchmarks Game uses it; min/max bracket
// the noise floor so callers can see whether the median is meaningful.
//
// A run that errors poisons the whole aggregate — we don't carry partial
// results forward because a flaky failure (e.g. python3 not found on one
// of the iterations) silently halving the data set is exactly the
// pathology median-of-runs is supposed to prevent.
func measure(runs int, fn func() result) aggregate {
	out := aggregate{Runs: runs}
	if runs <= 0 {
		runs = 1
	}
	durations := make([]float64, 0, runs)
	var sample result
	var maxMem int64
	for i := 0; i < runs; i++ {
		r := fn()
		if i == 0 {
			out.Program, out.N, out.Lang = r.Program, r.N, r.Lang
		}
		if r.Err != "" {
			out.Program, out.N, out.Lang, out.Err = r.Program, r.N, r.Lang, r.Err
			return out
		}
		durations = append(durations, r.DurationUs)
		if r.MemoryBytes > maxMem {
			maxMem = r.MemoryBytes
		}
		// Remember a sample for Output reporting; the table only shows
		// one Output per (program,n,lang) so we keep the last run's,
		// after the equality check has already happened across rows.
		sample = r
	}
	sort.Float64s(durations)
	out.MinUs = durations[0]
	out.MaxUs = durations[len(durations)-1]
	out.MedianUs = median(durations)
	out.MemoryBytes = maxMem
	out.Output = sample.Output
	return out
}

func median(sorted []float64) float64 {
	n := len(sorted)
	if n == 0 {
		return 0
	}
	if n%2 == 1 {
		return sorted[n/2]
	}
	return (sorted[n/2-1] + sorted[n/2]) / 2
}

func renderMarkdown(aggs []aggregate) string {
	type key struct {
		prog string
		n    int
	}
	groups := map[key]map[string]aggregate{}
	var orderedKeys []key
	seen := map[key]bool{}
	for _, a := range aggs {
		k := key{a.Program, a.N}
		if !seen[k] {
			seen[k] = true
			orderedKeys = append(orderedKeys, k)
		}
		if groups[k] == nil {
			groups[k] = map[string]aggregate{}
		}
		groups[k][a.Lang] = a
	}
	sort.Slice(orderedKeys, func(i, j int) bool {
		if orderedKeys[i].prog != orderedKeys[j].prog {
			return orderedKeys[i].prog < orderedKeys[j].prog
		}
		return orderedKeys[i].n < orderedKeys[j].n
	})

	var sb strings.Builder
	sb.WriteString("| Program | N | vm2 (µs) | CPython (µs) | PyPy (µs) | Lua (µs) | LuaJIT (µs) | Go (µs) | vm2 / Go | vm2 / CPython | vm2 / PyPy | vm2 / Lua | vm2 / LuaJIT | match |\n")
	sb.WriteString("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---:|\n")
	for _, k := range orderedKeys {
		row := groups[k]
		vm2 := row["vm2"]
		py := row["py"]
		pypy := row["pypy"]
		lua := row["lua"]
		luajit := row["luajit"]
		goRow := row["go"]
		cell := func(a aggregate) string {
			if a.Err != "" {
				return "ERR"
			}
			if a.MedianUs == 0 {
				return "—"
			}
			return fmt.Sprintf("%.0f", a.MedianUs)
		}
		ratio := func(num, den aggregate) string {
			if num.Err != "" || den.Err != "" || den.MedianUs == 0 {
				return "—"
			}
			return fmt.Sprintf("%.2fx", num.MedianUs/den.MedianUs)
		}
		match := "✓"
		out := func(a aggregate) string { return fmt.Sprintf("%v", a.Output) }
		check := func(a aggregate) bool {
			return a.Lang != "" && a.Err == "" && a.MedianUs > 0
		}
		if vm2.Err != "" {
			match = "ERR"
		} else {
			peers := []aggregate{py, pypy, lua, luajit, goRow}
			for _, peer := range peers {
				if !check(peer) {
					continue
				}
				if out(vm2) != out(peer) {
					match = fmt.Sprintf("✗ (vm2=%v %s=%v)", vm2.Output, peer.Lang, peer.Output)
					break
				}
			}
		}
		fmt.Fprintf(&sb, "| `%s` | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
			k.prog, k.n,
			cell(vm2), cell(py), cell(pypy), cell(lua), cell(luajit), cell(goRow),
			ratio(vm2, goRow), ratio(vm2, py), ratio(vm2, pypy), ratio(vm2, lua), ratio(vm2, luajit),
			match,
		)
	}
	return sb.String()
}

func formatBytes(n int64) string {
	const (
		kb = 1024
		mb = 1024 * kb
	)
	switch {
	case n >= mb:
		return fmt.Sprintf("%.1f MB", float64(n)/mb)
	case n >= kb:
		return fmt.Sprintf("%.0f KB", float64(n)/kb)
	default:
		return fmt.Sprintf("%d B", n)
	}
}

func die(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

// pypyBinary locates a pypy3 binary on PATH. macOS via brew installs
// `pypy3.11` (versioned), Linux distributions typically install plain
// `pypy3`. We check the versioned name first because that is what
// brew puts on PATH.
func pypyBinary() string {
	for _, name := range []string{"pypy3.11", "pypy3.12", "pypy3.10", "pypy3", "pypy"} {
		if p, err := exec.LookPath(name); err == nil {
			return p
		}
	}
	return "pypy3" // let exec fail with a clear error message
}
