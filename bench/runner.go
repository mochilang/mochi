package bench

import (
	"bytes"
	"embed"
	"encoding/json"
	"fmt"
	"github.com/fatih/color"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

//go:embed template/**/*
var templatesFS embed.FS

type Bench struct {
	Name    string
	Command []string
}

type Template struct {
	Lang    string
	Path    string
	Suffix  string
	Command []string
}

type Range struct {
	Start int
	Step  string // "x10", "+10000", "+10"
	Count int
}

type Result struct {
	Name       string  `json:"name"`
	DurationMs float64 `json:"duration_ms"`
	// Success    bool    `json:"success"`
	Output any `json:"output"`
	Lang   string
}

const keepTempFiles = false

func Benchmarks(tempDir string) []Bench {
	var benches []Bench

	_ = fs.WalkDir(templatesFS, "template", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return nil
		}

		ext := filepath.Ext(path)
		if ext == "" {
			return nil
		}
		lang := strings.TrimPrefix(ext, ".")
		if lang != "mochi" && lang != "py" && lang != "ts" && lang != "go" && lang != "go_tmpl" {
			return nil
		}

		parts := strings.Split(path, "/")
		if len(parts) < 4 {
			return nil
		}
		category := parts[1]
		name := parts[2]
		suffix := "." + lang
		cfg := Range{Start: 10, Step: "+10", Count: 3}

		// If mochi, run twice: once with interpreter, once with --vm
		if lang == "mochi" {
			benches = append(benches, generateBenchmarks(tempDir, category, name, cfg, []Template{
				{Lang: "mochi_interp", Path: path, Suffix: suffix, Command: []string{"mochi", "run"}},
				{Lang: "mochi_vm", Path: path, Suffix: suffix, Command: []string{"mochi", "run", "--vm"}},
			})...)
			return nil
		}

		// Other languages
		cmd := map[string][]string{
			"py":      {"python3"},
			"ts":      {"deno", "run", "--allow-read"},
			"go_tmpl": {"go", "run"},
		}[lang]
		if cmd == nil {
			panic("unsupported language: " + lang)
		}

		benches = append(benches, generateBenchmarks(tempDir, category, name, cfg,
			[]Template{{Lang: lang, Path: path, Suffix: suffix, Command: cmd}})...)
		return nil
	})

	return benches
}

func generateBenchmarks(tempDir, category, name string, cfg Range, templates []Template) []Bench {
	var benches []Bench

	for i := 0; i < cfg.Count; i++ {
		n := cfg.Start
		switch cfg.Step {
		case "x10":
			n *= pow(10, i)
		case "+10000":
			n += i * 10000
		case "+10":
			n += i * 10
		}

		for _, t := range templates {
			actualSuffix := t.Suffix
			if actualSuffix == ".go_tmpl" {
				actualSuffix = ".go"
			}

			fileName := fmt.Sprintf("%s_%s_%d%s", category, name, n, actualSuffix)
			out := filepath.Join(tempDir, fileName)

			render(t.Path, map[string]string{
				"N": fmt.Sprintf("%d", n),
			}, out)

			absOut, err := filepath.Abs(out)
			if err != nil {
				panic(err)
			}

			benches = append(benches, Bench{
				Name:    fmt.Sprintf("%s.%s.%d.%s", category, name, n, t.Lang),
				Command: append(t.Command, absOut),
			})
		}
	}
	return benches
}
func Run() {
	color.Cyan("📊 Mochi Benchmarks")

	tempDir, err := os.MkdirTemp(os.TempDir(), "mochi-bench-*")
	if err != nil {
		panic(err)
	}
	if !keepTempFiles {
		defer func() {
			if err := os.RemoveAll(tempDir); err != nil {
				fmt.Fprintf(os.Stderr, "⚠️ Failed to remove temp dir %q: %v\n", tempDir, err)
			}
		}()
	} else {
		fmt.Println("🔎 Temp files kept at:", tempDir)
	}

	benches := Benchmarks(tempDir)
	var results []Result

	for _, b := range benches {
		script := b.Command[len(b.Command)-1]
		if _, err := os.Stat(script); err != nil {
			fmt.Printf("❗ %-32s (file missing)\n", b.Name)
			continue
		}

		var stdout, stderr bytes.Buffer
		cmd := exec.Command(b.Command[0], b.Command[1:]...)
		cmd.Env = append(os.Environ(), "GO111MODULE=off")
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		cmd.Dir = tempDir

		externalStart := timeNowMs()
		err := cmd.Run()
		externalDuration := timeNowMs() - externalStart

		if err != nil {
			fmt.Printf("❌ %-32s (err: %v)\n", b.Name, err)
			if stderr.Len() > 0 {
				fmt.Println("🔻 stderr:")
				fmt.Println(indent(stderr.String()))
			}
			if stdout.Len() > 0 {
				fmt.Println("📤 stdout:")
				fmt.Println(indent(stdout.String()))
			}
			continue
		}

		var r Result
		if err := json.Unmarshal(stdout.Bytes(), &r); err != nil {
			fmt.Printf("❌ %-32s (invalid json)\n", b.Name)
			continue
		}

		r.Name = b.Name
		r.Lang = strings.Split(b.Name, ".")[3]
		results = append(results, r)

		fmt.Printf("✅ %-32s ⏱ %.3fms (ext: %.2fms)\n",
			r.Name,
			r.DurationMs,     // internal timing
			externalDuration) // outer exec timing
	}

	report(results)
}

func indent(s string) string {
	lines := strings.Split(strings.TrimSpace(s), "\n")
	for i, line := range lines {
		lines[i] = "    " + line
	}
	return strings.Join(lines, "\n")
}

func report(results []Result) {
	grouped := map[string][]Result{}

	// Group by category.name.size (drop Lang suffix)
	for _, r := range results {
		parts := strings.Split(r.Name, ".")
		if len(parts) < 4 {
			fmt.Printf("⚠️ Skipping malformed result name: %s\n", r.Name)
			continue
		}
		group := strings.Join(parts[:3], ".") // e.g., "math.fact_rec.10"
		grouped[group] = append(grouped[group], r)
	}

	// Sort group names
	var groups []string
	for group := range grouped {
		groups = append(groups, group)
	}
	sort.Strings(groups)

	// Print each group
	for _, group := range groups {
		set := grouped[group]
		fmt.Printf("\n📦 %s\n", group)

		sort.Slice(set, func(i, j int) bool {
			return set[i].DurationMs < set[j].DurationMs
		})

		best := set[0].DurationMs
		for _, r := range set {
			delta := r.DurationMs - best
			plus := ""
			if delta > 0 {
				plus = fmt.Sprintf(" +%.1f%%", (delta/best)*100)
			}

			// Friendly label
			langName := r.Lang
			switch langName {
			case "mochi_interp":
				langName = "mochi (interp)"
			case "mochi_vm":
				langName = "mochi (vm)"
			case "go_tmpl":
				langName = "go"
			}

			status := "✓"
			fmt.Printf("  %s %-24s %8.4fms  %s\n",
				color.New(color.FgGreen).Sprint(status),
				langName,
				r.DurationMs,
				color.New(color.FgCyan).Sprint("✓ Best")+plus)
		}
	}
}

func render(path string, data map[string]string, out string) {
	src, err := templatesFS.ReadFile(path)
	if err != nil {
		panic(err)
	}
	code := string(src)
	for k, v := range data {
		code = strings.ReplaceAll(code, "{{ ."+k+" }}", v)
	}
	err = os.WriteFile(out, []byte(code), 0644)
	if err != nil {
		panic(err)
	}
}

func pow(base, exp int) int {
	result := 1
	for i := 0; i < exp; i++ {
		result *= base
	}
	return result
}

func timeNowMs() float64 {
	return float64(time.Now().UnixNano()) / 1e6
}
