//go:build rosetta

package ocaml_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	ocaml "mochi/transpiler/x/ocaml"
        "mochi/types"
)

var update = flag.Bool("update-rosetta-ocaml", false, "update golden files")

func repoRootDir(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

// shouldUpdate reports whether the -update flag was set.
func shouldUpdate() bool {
       return *update
}

func runCase(src, outDir string) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".ml")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, errs[0]
	}
	ocaml.SetBenchMain(bench)
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	code := ast.Emit()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	exe := filepath.Join(outDir, base)
       if out, err := exec.Command("ocamlc", "-I", "+zarith", "-I", "+sha", "zarith.cma", "sha.cma", "unix.cma", codePath, "-o", exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, err
	}
	cmd := exec.Command(exe)
	runEnv := append(os.Environ())
	if bench {
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
	} else {
		runEnv = append(runEnv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = runEnv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, err
	}
	outBytes := bytes.TrimSpace(out)
	if bench {
		if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
			outBytes = outBytes[idx:]
		}
		_ = os.WriteFile(benchPath, outBytes, 0o644)
	} else {
		_ = os.WriteFile(outPath, outBytes, 0o644)
	}
	_ = os.Remove(errPath)
	return outBytes, nil
}

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func TestOCamlTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skip("ocamlc not installed")
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	root := repoRootDir(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "OCaml")
	os.MkdirAll(outDir, 0o755)

	idx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 {
			idx = n
		}
	}
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if idx > len(names) {
		t.Fatalf("index %d out of range", idx)
	}
	nameFile := names[idx-1]
	name := strings.TrimSuffix(nameFile, ".mochi")
	src := filepath.Join(srcDir, nameFile)

	if ok := t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) {
		got, err := runCase(src, outDir)
		if err != nil {
			t.Fatalf("%v", err)
		}
		if bench {
			// no golden comparison when benchmarking
			return
		}
		wantPath := filepath.Join(outDir, name+".out")
		want, err := os.ReadFile(wantPath)
		if err != nil {
			if shouldUpdate() {
				if err2 := os.WriteFile(wantPath, append(got, '\n'), 0o644); err2 == nil {
					t.Logf("updated: %s", wantPath)
					return
				} else {
					t.Fatalf("write golden: %v", err2)
				}
			}
			t.Fatalf("read golden: %v", err)
		}
		got = bytes.TrimSpace(got)
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			if shouldUpdate() {
				if err2 := os.WriteFile(wantPath, append(got, '\n'), 0o644); err2 == nil {
					t.Logf("updated: %s", wantPath)
					return
				} else {
					t.Fatalf("write golden: %v", err2)
				}
			}
			t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
		}
	}); !ok {
		t.Fatalf("program failed: %s", name)
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateRosettaReadme()
	os.Exit(code)
}

func updateRosettaReadme() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "OCaml")
	readmePath := filepath.Join(root, "transpiler", "x", "ocaml", "ROSETTA.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".ml")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}

	var buf bytes.Buffer
	buf.WriteString("# Rosetta OCaml Transpiler\n\n")
	buf.WriteString("This directory contains OCaml code generated from Rosetta Code programs in `tests/rosetta/x/Mochi`.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			buf.WriteString("Last updated " + t.Format("2006-01-02 15:04 MST") + "\n")
		}
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
