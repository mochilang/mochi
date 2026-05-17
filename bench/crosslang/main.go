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
	flag.Parse()

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
		for _, n := range p.ns {
			aggs = append(aggs,
				measure(*repeat, func() result { return runVM2(vm2Bin, p.cat(), p.name, n) }),
				measure(*repeat, func() result { return runNative(repoRoot, tmp, p.cat(), p.name, n, "py", "python3") }),
				measure(*repeat, func() result { return runNative(repoRoot, tmp, p.cat(), p.name, n, "lua", "lua") }),
				measure(*repeat, func() result { return runGo(repoRoot, tmp, p.cat(), p.name, n) }),
			)
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

func runNative(repoRoot, tmp, cat, prog string, n int, suffix, interpreter string) result {
	srcPath := filepath.Join(repoRoot, "bench", "template", cat, prog, prog+"."+suffix)
	src, err := os.ReadFile(srcPath)
	if err != nil {
		return result{Program: cat + "/" + prog, N: n, Lang: suffix, Err: err.Error()}
	}
	rendered := strings.ReplaceAll(string(src), "{{ .N }}", fmt.Sprintf("%d", n))
	out := filepath.Join(tmp, fmt.Sprintf("%s_%s_%d.%s", cat, prog, n, suffix))
	if err := os.WriteFile(out, []byte(rendered), 0644); err != nil {
		return result{Program: cat + "/" + prog, N: n, Lang: suffix, Err: err.Error()}
	}
	cmd := exec.Command(interpreter, out)
	return runCmd(cat, prog, n, suffix, cmd)
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
	sb.WriteString("| Program | N | vm2 (µs) | CPython (µs) | Lua (µs) | Go (µs) | vm2 / CPython | vm2 / Lua | vm2 / Go | vm2 RSS | CPython RSS | Lua RSS | Go RSS | match |\n")
	sb.WriteString("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---:|\n")
	for _, k := range orderedKeys {
		row := groups[k]
		vm2 := row["vm2"]
		py := row["py"]
		lua := row["lua"]
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
		rss := func(a aggregate) string {
			if a.MemoryBytes <= 0 {
				return "—"
			}
			return formatBytes(a.MemoryBytes)
		}
		match := "✓"
		out := func(a aggregate) string { return fmt.Sprintf("%v", a.Output) }
		// Go is optional: the row may not have a .go peer yet. Only
		// participate in the output-equality check when present and
		// non-error so missing .go files do not flip the match flag.
		hasGo := goRow.Lang == "go" && goRow.Err == ""
		if vm2.Err != "" || py.Err != "" || lua.Err != "" {
			match = "ERR"
		} else if out(vm2) != out(py) || out(vm2) != out(lua) ||
			(hasGo && out(vm2) != out(goRow)) {
			match = fmt.Sprintf("✗ (vm2=%v py=%v lua=%v go=%v)", vm2.Output, py.Output, lua.Output, goRow.Output)
		}
		fmt.Fprintf(&sb, "| `%s` | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
			k.prog, k.n,
			cell(vm2), cell(py), cell(lua), cell(goRow),
			ratio(vm2, py), ratio(vm2, lua), ratio(vm2, goRow),
			rss(vm2), rss(py), rss(lua), rss(goRow),
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
