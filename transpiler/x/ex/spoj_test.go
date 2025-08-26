//go:build slow

package ex_test

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

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
	"mochi/types"
)

func TestExTranspiler_Spoj_Golden(t *testing.T) {
	ensureElixir(t)
	root := repoRoot(t)
	t.Cleanup(updateSpoj)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "elixir")
	os.MkdirAll(outDir, 0o755)

	files, _ := filepath.Glob(filepath.Join(outDir, "*.out"))
	sort.Slice(files, func(i, j int) bool {
		ai := strings.TrimSuffix(filepath.Base(files[i]), ".out")
		aj := strings.TrimSuffix(filepath.Base(files[j]), ".out")
		ii, _ := strconv.Atoi(ai)
		ij, _ := strconv.Atoi(aj)
		return ii < ij
	})

	bench := os.Getenv("MOCHI_BENCHMARK") != ""

	for _, outFile := range files {
		base := strings.TrimSuffix(filepath.Base(outFile), ".out")
		src := filepath.Join(srcDir, base+".mochi")
		t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".exs")
			errPath := filepath.Join(outDir, base+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := ex.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ex.Emit(ast, bench)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("elixir", codePath)
			envv := os.Environ()
			if bench {
				envv = append(envv, "MOCHI_BENCHMARK=true")
			} else {
				envv = append(envv, "MOCHI_NOW_SEED=1")
			}
			inPath := filepath.Join(outDir, base+".in")
			if data, err := os.ReadFile(inPath); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			cmd.Env = envv
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			err = cmd.Run()
			got := bytes.TrimSpace(stdout.Bytes())
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), stderr.Bytes()...), 0o644)
				if !bench {
					t.Fatalf("run: %v", err)
				}
			} else {
				_ = os.Remove(errPath)
			}
			if bench {
				benchPath := filepath.Join(outDir, base+".bench")
				_ = os.WriteFile(benchPath, got, 0o644)
				return
			}
			want, err := os.ReadFile(outFile)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
			}
		})
	}
}

func updateSpoj() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "elixir")
	mdPath := filepath.Join(root, "transpiler", "x", "ex", "SPOJ.md")

	files, _ := filepath.Glob(filepath.Join(outDir, "*.out"))
	sort.Slice(files, func(i, j int) bool {
		ai := strings.TrimSuffix(filepath.Base(files[i]), ".out")
		aj := strings.TrimSuffix(filepath.Base(files[j]), ".out")
		ii, _ := strconv.Atoi(ai)
		ij, _ := strconv.Atoi(aj)
		return ii < ij
	})

	rows := []string{
		"| Index | Name | Status | Duration | Memory |",
		"|------:|------|:-----:|---------:|-------:|",
	}
	for _, outFile := range files {
		base := strings.TrimSuffix(filepath.Base(outFile), ".out")
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			line := strings.SplitN(string(data), "\n", 2)[0]
			line = strings.TrimPrefix(line, "#")
			line = strings.TrimSpace(line)
			if strings.HasPrefix(line, "[") {
				if idx := strings.Index(line, "]"); idx >= 0 {
					name = line[1:idx]
				} else {
					name = line
				}
			} else {
				name = line
			}
		}
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, base+".exs")); err == nil {
			status = "✓"
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
			var r struct {
				DurationUS int64 `json:"duration_us"`
				Memory     int64 `json:"memory"`
			}
			if json.Unmarshal(data, &r) == nil {
				dur = fmt.Sprintf("%.2fms", float64(r.DurationUS)/1000)
				if r.Memory >= 1024*1024 {
					mem = fmt.Sprintf("%.1f MB", float64(r.Memory)/1024/1024)
				} else if r.Memory > 0 {
					mem = fmt.Sprintf("%d B", r.Memory)
				} else {
					mem = "0 B"
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", base, name, status, dur, mem))
	}
	_ = os.WriteFile(mdPath, []byte(strings.Join(rows, "\n")+"\n"), 0o644)
}
