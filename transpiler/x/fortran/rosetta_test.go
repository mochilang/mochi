//go:build slow

package fortran_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	ftn "mochi/transpiler/x/fortran"
	"mochi/types"
)

func runTask(name string) ([]byte, error) {
	root := repoRoot(&testing.T{})
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")

	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type: %v", errs[0])
	}
	bench := os.Getenv("MOCHI_BENCHMARK") != "" && os.Getenv("MOCHI_BENCHMARK") != "0"
	ftn.SetBenchMain(bench)
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("transpile: %w", err)
	}

	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Fortran")
	os.MkdirAll(outDir, 0o755)

	f90Path := filepath.Join(outDir, name+".f90")
	if err := os.WriteFile(f90Path, ast.Emit(), 0o644); err != nil {
		return nil, err
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, fmt.Errorf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if bench {
		cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
	}
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, fmt.Errorf("run: %v", err)
	}
	outBytes := bytes.TrimSpace(out)
	benchPath := filepath.Join(outDir, name+".bench")
	outPath := filepath.Join(outDir, name+".out")
	if bench {
		benchData := outBytes
		if idx := bytes.LastIndex(benchData, []byte("{")); idx >= 0 {
			benchData = benchData[idx:]
		}
		_ = os.WriteFile(benchPath, benchData, 0o644)
		_ = os.Remove(outPath)
	} else {
		_ = os.WriteFile(outPath, outBytes, 0o644)
		_ = os.Remove(benchPath)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	return outBytes, nil
}

func TestFortranTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	t.Cleanup(updateChecklist)
	root := repoRoot(t)

	names, err := listPrograms(root)
	if err != nil {
		t.Fatalf("list programs: %v", err)
	}

	idx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			idx = n
		}
	}
	if idx < 1 || idx > len(names) {
		t.Fatalf("index %d out of range (1-%d)", idx, len(names))
	}
	name := names[idx-1]
	t.Logf("running #%d: %s", idx, name)

	bench := os.Getenv("MOCHI_BENCHMARK") != "" && os.Getenv("MOCHI_BENCHMARK") != "0"
	got, err := runTask(name)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if !bench {
		wantPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".out")
		if want, err := os.ReadFile(wantPath); err == nil {
			if !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
			}
		}
	}
}

func listPrograms(root string) ([]string, error) {
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	names := make([]string, len(files))
	for i, f := range files {
		names[i] = strings.TrimSuffix(filepath.Base(f), ".mochi")
	}
	return names, nil
}

func updateChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Fortran")
	readme := filepath.Join(root, "transpiler", "x", "fortran", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := ""
		dur := ""
		mem := ""
		benchPath := filepath.Join(outDir, name+".bench")
		outPath := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(benchPath); err == nil {
			status = "✓"
			compiled++
			var js struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil {
				if js.Dur > 0 {
					dur = humanDur(time.Duration(js.Dur) * time.Microsecond)
				}
				if js.Mem > 0 {
					mem = humanSize(js.Mem)
				}
			}
		} else if data, err := os.ReadFile(outPath); err == nil {
			status = "✓"
			compiled++
			var js struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
			}
			if json.Unmarshal(trimmed, &js) == nil && js.Dur > 0 {
				dur = humanDur(time.Duration(js.Dur) * time.Microsecond)
				mem = humanSize(js.Mem)
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); err == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04:05 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04:05 -0700")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Rosetta Fortran Transpiler Checklist\n\n")
	buf.WriteString("This checklist tracks Mochi programs from `tests/rosetta/x/Mochi` that successfully transpile using the experimental Fortran backend.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d):\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		fmt.Fprintf(&buf, "\n_Last updated: %s_\n", ts)
	}
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func humanDur(d time.Duration) string {
	if d < time.Millisecond {
		return fmt.Sprintf("%dus", d.Microseconds())
	}
	if d < time.Second {
		return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000)
	}
	if d < time.Minute {
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
	return d.String()
}

func humanSize(n int64) string {
	const unit = 1024
	units := []string{"B", "KB", "MB", "GB", "TB"}
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	val := float64(n)
	exp := 0
	for val >= unit && exp < len(units)-1 {
		val /= unit
		exp++
	}
	return fmt.Sprintf("%.1f %s", val, units[exp])
}
