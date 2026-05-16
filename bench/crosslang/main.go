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

func main() {
	outJSON := flag.String("json", "", "write per-result JSON array to this file")
	outMD := flag.String("md", "", "write markdown summary to this file")
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

	var results []result
	for _, p := range programs {
		for _, n := range p.ns {
			results = append(results, runVM2(vm2Bin, p.cat(), p.name, n))
			results = append(results, runNative(repoRoot, tmp, p.cat(), p.name, n, "py", "python3"))
			results = append(results, runNative(repoRoot, tmp, p.cat(), p.name, n, "lua", "lua"))
		}
	}

	// Print live progress + final table.
	for _, r := range results {
		if r.Err != "" {
			fmt.Printf("[%s n=%d] %-6s ERR: %s\n", r.Program, r.N, r.Lang, r.Err)
			continue
		}
		fmt.Printf("[%s n=%d] %-6s %10.0f us  %8d B  -> %v\n",
			r.Program, r.N, r.Lang, r.DurationUs, r.MemoryBytes, r.Output)
	}

	if *outJSON != "" {
		f, err := os.Create(*outJSON)
		if err != nil {
			die("create json: %v", err)
		}
		enc := json.NewEncoder(f)
		enc.SetIndent("", "  ")
		if err := enc.Encode(results); err != nil {
			die("encode json: %v", err)
		}
		f.Close()
	}

	md := renderMarkdown(results)
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

func renderMarkdown(results []result) string {
	type key struct {
		prog string
		n    int
	}
	groups := map[key]map[string]result{}
	var orderedKeys []key
	seen := map[key]bool{}
	for _, r := range results {
		k := key{r.Program, r.N}
		if !seen[k] {
			seen[k] = true
			orderedKeys = append(orderedKeys, k)
		}
		if groups[k] == nil {
			groups[k] = map[string]result{}
		}
		groups[k][r.Lang] = r
	}
	sort.Slice(orderedKeys, func(i, j int) bool {
		if orderedKeys[i].prog != orderedKeys[j].prog {
			return orderedKeys[i].prog < orderedKeys[j].prog
		}
		return orderedKeys[i].n < orderedKeys[j].n
	})

	var sb strings.Builder
	sb.WriteString("| Program | N | vm2 (µs) | CPython (µs) | Lua (µs) | vm2 / CPython | vm2 / Lua | vm2 RSS | CPython RSS | Lua RSS | match |\n")
	sb.WriteString("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---:|\n")
	for _, k := range orderedKeys {
		row := groups[k]
		vm2 := row["vm2"]
		py := row["py"]
		lua := row["lua"]
		cell := func(r result) string {
			if r.Err != "" {
				return "ERR"
			}
			if r.DurationUs == 0 {
				return "—"
			}
			return fmt.Sprintf("%.0f", r.DurationUs)
		}
		ratio := func(num, den result) string {
			if num.Err != "" || den.Err != "" || den.DurationUs == 0 {
				return "—"
			}
			return fmt.Sprintf("%.2fx", num.DurationUs/den.DurationUs)
		}
		rss := func(r result) string {
			if r.MemoryBytes <= 0 {
				return "—"
			}
			return formatBytes(r.MemoryBytes)
		}
		match := "✓"
		// Compare outputs as canonical Go-formatted strings; treat
		// missing values as non-matching.
		out := func(r result) string { return fmt.Sprintf("%v", r.Output) }
		if vm2.Err != "" || py.Err != "" || lua.Err != "" {
			match = "ERR"
		} else if out(vm2) != out(py) || out(vm2) != out(lua) {
			match = fmt.Sprintf("✗ (vm2=%v py=%v lua=%v)", vm2.Output, py.Output, lua.Output)
		}
		fmt.Fprintf(&sb, "| `%s` | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
			k.prog, k.n,
			cell(vm2), cell(py), cell(lua),
			ratio(vm2, py), ratio(vm2, lua),
			rss(vm2), rss(py), rss(lua),
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
