//go:build archived

package bench

import (
	"bytes"
	"embed"
	"encoding/json"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/fatih/color"

	gocode "mochi/archived/go"
	pycode "mochi/archived/py"
	tscode "mochi/archived/ts"
	ccode "mochi/archived/x/c"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
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
	DurationUs float64 `json:"duration_us"`
	// Success    bool    `json:"success"`
	Output any `json:"output"`
	Lang   string
}

const keepTempFiles = false

func Benchmarks(tempDir, mochiBin string) []Bench {
	var benches []Bench

	_ = fs.WalkDir(templatesFS, "template", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return nil
		}

		ext := filepath.Ext(path)
		if ext != ".mochi" {
			return nil
		}
		lang := strings.TrimPrefix(ext, ".")

		parts := strings.Split(path, "/")
		if len(parts) < 4 {
			return nil
		}
		category := parts[1]
		name := parts[2]
		if category == "join" {
			return nil
		}
		suffix := "." + lang
		cfg := Range{Start: 10, Step: "+10", Count: 3}

		templates := []Template{
			{Lang: "mochi_vm", Path: path, Suffix: suffix, Command: []string{"go", "run"}},
			{Lang: "mochi_go", Path: path, Suffix: suffix, Command: []string{"go", "run"}},
		}
		templates = append(templates, Template{Lang: "mochi_c", Path: path, Suffix: suffix, Command: nil})
		templates = append(templates, Template{Lang: "mochi_py", Path: path, Suffix: suffix, Command: []string{"python3"}})
		// Temporarily disable PyPy and Cython benchmarks
		// templates = append(templates, Template{Lang: "mochi_pypy", Path: path, Suffix: suffix, Command: []string{"pypy3"}})
		// templates = append(templates, Template{Lang: "mochi_cython", Path: path, Suffix: suffix, Command: nil})
		templates = append(templates, Template{Lang: "mochi_ts", Path: path, Suffix: suffix, Command: []string{"deno", "run", "--quiet"}})

		benches = append(benches, generateBenchmarks(tempDir, category, name, cfg, templates)...)
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

			fileName := fmt.Sprintf("%s_%s_%d%s", category, name, n, actualSuffix)
			out := filepath.Join(tempDir, fileName)

			render(t.Path, map[string]string{
				"N": fmt.Sprintf("%d", n),
			}, out)

			// If this benchmark compiles Mochi to another language, do it now.
			if t.Lang == "mochi_go" {
				compiled := strings.TrimSuffix(out, ".mochi") + ".go"
				if err := compileToGo(out, compiled); err != nil {
					panic(err)
				}
				out = compiled
			} else if t.Lang == "mochi_vm" {
				compiled := strings.TrimSuffix(out, ".mochi") + ".go"
				if err := compileToVM(out, compiled); err != nil {
					panic(err)
				}
				out = compiled
			} else if t.Lang == "mochi_ts" {
				compiled := strings.TrimSuffix(out, ".mochi") + ".ts"
				if err := compileToTs(out, compiled); err != nil {
					panic(err)
				}
				out = compiled
			} else if t.Lang == "mochi_c" {
				compiled := strings.TrimSuffix(out, ".mochi") + ".c"
				bin := strings.TrimSuffix(out, ".mochi")
				if err := compileToC(out, compiled, bin); err != nil {
					panic(err)
				}
				out = bin
			} else if t.Lang == "mochi_py" {
				compiled := strings.TrimSuffix(out, ".mochi") + ".py"
				if err := compileToPy(out, compiled); err != nil {
					panic(err)
				}
				out = compiled
				// } else if t.Lang == "mochi_cython" {
				//      compiled := strings.TrimSuffix(out, ".mochi") + ".cy.c"
				//      bin := strings.TrimSuffix(out, ".mochi") + ".cy.bin"
				//      if err := compileToCython(out, compiled, bin); err != nil {
				//              panic(err)
				//      }
				//      out = bin
			}

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

	mochiBin, err := gocode.EnsureMochi()
	if err != nil {
		panic(err)
	}
	if err := pycode.EnsurePython(); err != nil {
		panic(err)
	}
	// Temporarily disable PyPy and Cython benchmarks
	// if err := pycode.EnsurePyPy(); err != nil {
	//      panic(err)
	// }
	// if err := pycode.EnsureCython(); err != nil {
	//      panic(err)
	// }
	if err := tscode.EnsureDeno(); err != nil {
		panic(err)
	}
	// Skip C compiler check in this environment
	benches := Benchmarks(tempDir, mochiBin)
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

		externalStart := timeNowUs()
		err := cmd.Run()
		externalDuration := timeNowUs() - externalStart

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
			fmt.Printf("❌ %-32s (invalid json: %v)\n", b.Name, err)
			if stdout.Len() > 0 {
				fmt.Println("📤 stdout:")
				fmt.Println(indent(stdout.String()))
			}
			if stderr.Len() > 0 {
				fmt.Println("🔻 stderr:")
				fmt.Println(indent(stderr.String()))
			}
			continue
		}

		r.Name = b.Name
		r.Lang = strings.Split(b.Name, ".")[3]
		results = append(results, r)

		fmt.Printf("✅ %-32s ⏱ %.0fµs (ext: %.2fµs)\n",
			r.Name,
			r.DurationUs,     // internal timing
			externalDuration) // outer exec timing
	}

	report(results)
	if err := exportMarkdown(results); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write BENCHMARK.md: %v\n", err)
	}
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
			return set[i].DurationUs < set[j].DurationUs
		})

		best := set[0].DurationUs
		for _, r := range set {
			delta := r.DurationUs - best
			plus := ""
			if delta > 0 {
				plus = fmt.Sprintf(" +%.1f%%", (float64(delta)/float64(best))*100)
			}

			// Friendly label
			langName := r.Lang
			switch langName {
			case "mochi_vm":
				langName = "Mochi (VM)"
			case "mochi_go":
				langName = "Mochi (Go)"
			case "mochi_c":
				langName = "C"
			case "mochi_py":
				langName = "Python"
				// case "mochi_pypy":
				//      langName = "Python (PyPy)"
				// case "mochi_cython":
				//      langName = "Python (Cython)"
			case "mochi_ts":
				langName = "Typescript"
			}

			status := "✓"
			fmt.Printf("  %s %-24s %8.0fµs  %s\n",
				color.New(color.FgGreen).Sprint(status),
				langName,
				r.DurationUs,
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

func compileToGo(mochiFile, goFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := gocode.New(typeEnv)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	return os.WriteFile(goFile, code, 0644)
}

func compileToPy(mochiFile, pyFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := pycode.New(typeEnv)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	return os.WriteFile(pyFile, code, 0644)
}

func compileToTs(mochiFile, tsFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := tscode.New(typeEnv, filepath.Dir(mochiFile))
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	return os.WriteFile(tsFile, code, 0644)
}

func compileToC(mochiFile, cFile, binFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := ccode.New(typeEnv)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	if err := os.WriteFile(cFile, code, 0644); err != nil {
		return err
	}
	cc, err := ccode.EnsureCC()
	if err != nil {
		return err
	}
	cmd := exec.Command(cc, "-x", "c", cFile, "-O2", "-std=c99", "-D_POSIX_C_SOURCE=199309L", "-lrt", "-o", binFile)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("cc error: %w", err)
	}
	return nil
}

func compileToCython(mochiFile, cFile, binFile string) error {
	base := strings.TrimSuffix(cFile, ".cy.c")
	pyFile := base + ".py"
	if err := compileToPy(mochiFile, pyFile); err != nil {
		return err
	}
	cmd := exec.Command("cython3", "--embed", "-3", "-o", cFile, pyFile)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	args := fmt.Sprintf("gcc -O2 %s $(python3-config --cflags --embed --ldflags) -o %s", cFile, binFile)
	cmd = exec.Command("sh", "-c", args)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func compileToVM(mochiFile, goFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, typeEnv)
	if err != nil {
		return err
	}
	data, err := json.Marshal(p)
	if err != nil {
		return err
	}
	var b strings.Builder
	fmt.Fprintln(&b, "package main")
	fmt.Fprintln(&b, "import (\n\t\"encoding/json\"\n\t\"os\"\n\t\"mochi/runtime/vm\"\n)")
	fmt.Fprintf(&b, "var progData = []byte(`%s`)\n", string(data))
	fmt.Fprintln(&b, "func main() {")
	fmt.Fprintln(&b, "\tvar p vm.Program")
	fmt.Fprintln(&b, "\tif err := json.Unmarshal(progData, &p); err != nil { panic(err) }")
	fmt.Fprintln(&b, "\tm := vm.New(&p, os.Stdout)")
	fmt.Fprintln(&b, "\tif err := m.Run(); err != nil { panic(err) }")
	fmt.Fprintln(&b, "}")
	return os.WriteFile(goFile, []byte(b.String()), 0644)
}

func compileToIR(mochiFile, irFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return err
	}
	src, err := os.ReadFile(mochiFile)
	if err != nil {
		return err
	}
	ir := p.Disassemble(string(src))
	return os.WriteFile(irFile, []byte(ir), 0644)
}

func timeNowUs() float64 {
	return float64(time.Now().UnixNano()) / 1e3
}

func exportMarkdown(results []Result) error {
	var b strings.Builder
	b.WriteString("# Benchmarks\n\n")

	grouped := map[string][]Result{}
	for _, r := range results {
		parts := strings.Split(r.Name, ".")
		if len(parts) < 4 {
			continue
		}
		group := strings.Join(parts[:3], ".")
		grouped[group] = append(grouped[group], r)
	}

	var groups []string
	for g := range grouped {
		groups = append(groups, g)
	}
	sort.Strings(groups)

	for _, g := range groups {
		set := grouped[g]
		sort.Slice(set, func(i, j int) bool {
			return set[i].DurationUs < set[j].DurationUs
		})

		b.WriteString("## " + g + "\n")
		b.WriteString("| Language | Time (µs) | +/- |\n")
		b.WriteString("| --- | ---: | --- |\n")

		best := set[0].DurationUs
		for _, r := range set {
			delta := r.DurationUs - best
			plus := "best"
			if delta > 0 {
				plus = fmt.Sprintf("+%.1f%%", (float64(delta)/float64(best))*100)
			}

			langName := r.Lang
			switch langName {
			case "mochi_vm":
				langName = "Mochi (VM)"
			case "mochi_go":
				langName = "Mochi (Go)"
			case "mochi_c":
				langName = "C"
			case "mochi_py":
				langName = "Python"
				// case "mochi_pypy":
				//      langName = "Python (PyPy)"
				// case "mochi_cython":
				//      langName = "Python (Cython)"
			case "mochi_ts":
				langName = "Typescript"
			}
			b.WriteString(fmt.Sprintf("| %s | %.0f | %s |\n", langName, r.DurationUs, plus))
		}

		b.WriteString("\n")
	}

	return os.WriteFile("BENCHMARK.md", []byte(b.String()), 0644)
}

// GenerateOutputs compiles all benchmark templates to Go, Python and TypeScript under
// outDir. It mirrors the functionality previously provided by the
// gen-bench-out command.
func GenerateOutputs(outDir string) error {
	base := "template"

	os.RemoveAll(outDir)
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return err
	}

	return fs.WalkDir(templatesFS, base, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		if filepath.Ext(path) != ".mochi" {
			return nil
		}

		parts := strings.Split(path, "/")
		if len(parts) < 4 {
			return nil
		}
		category := parts[1]
		name := strings.TrimSuffix(parts[2], "")
		if category == "join" {
			return nil
		}

		for _, n := range []int{10, 20, 30} {
			tmp := filepath.Join(outDir, "tmp.mochi")
			render(path, map[string]string{"N": fmt.Sprintf("%d", n)}, tmp)

			goOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.go.out", category, name, n))
			if err := compileToGo(tmp, goOut); err != nil {
				return err
			}

			vmOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.vm.go.out", category, name, n))
			if err := compileToVM(tmp, vmOut); err != nil {
				return err
			}

			irOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.ir.out", category, name, n))
			if err := compileToIR(tmp, irOut); err != nil {
				return err
			}

			if category != "join" {
				cOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.c.out", category, name, n))
				bin := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.c.bin", category, name, n))
				if err := compileToC(tmp, cOut, bin); err != nil {
					return err
				}
				os.Remove(bin)
			}

			pyOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.py.out", category, name, n))
			if err := compileToPy(tmp, pyOut); err != nil {
				return err
			}

			tsOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.ts.out", category, name, n))
			if err := compileToTs(tmp, tsOut); err != nil {
				return err
			}

			os.Remove(tmp)
		}
		return nil
	})
}
