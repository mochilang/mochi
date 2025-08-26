//go:build slow

package ctrans_test

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

	ctrans "mochi/transpiler/x/c"
)

func TestCTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "c")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJReadme)

	bench := os.Getenv("MOCHI_BENCHMARK") != ""
	ctrans.SetBenchMain(bench)

	pattern := filepath.Join(outDir, "*.out")
	outFiles, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob out files: %v", err)
	}
	if len(outFiles) == 0 {
		t.Fatalf("no SPOJ test cases found in %s", outDir)
	}
	sort.Strings(outFiles)
	for _, of := range outFiles {
		outFile := of
		base := strings.TrimSuffix(filepath.Base(outFile), ".out")
		idx, _ := strconv.Atoi(base)
		t.Run(fmt.Sprintf("%03d", idx), func(t *testing.T) {
			src := filepath.Join(srcDir, base+".mochi")
			inPath := filepath.Join(outDir, base+".in")
			codePath := filepath.Join(outDir, base+".c")
			errPath := filepath.Join(outDir, base+".error")
			benchPath := filepath.Join(outDir, base+".bench")

			want, err := os.ReadFile(outFile)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			if !bench {
				want = bytes.TrimSpace(want)
			}

			code, err := transpileFile(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cc, err := ctrans.EnsureCC()
			if err != nil {
				t.Fatalf("ensure cc: %v", err)
			}
			exe := filepath.Join(outDir, base)
			defer os.Remove(exe)
			args := []string{codePath, "-o", exe}
			if bytes.Contains(code, []byte("<math.h>")) {
				args = append(args, "-lm")
			}
			if bytes.Contains(code, []byte("<gmp.h>")) {
				args = append(args, "-lgmp")
			}
			if out, err := exec.Command(cc, args...).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			cmd := exec.Command(exe)
			envv := os.Environ()
			if bench {
				envv = append(envv, "MOCHI_BENCHMARK=1")
			} else {
				envv = append(envv, "MOCHI_NOW_SEED=1")
			}
			cmd.Env = envv
			if data, err := os.ReadFile(inPath); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)

			if bench {
				if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
					if updateEnabled() {
						_ = os.WriteFile(outFile, bytes.TrimSpace(got[:idx]), 0o644)
						_ = os.WriteFile(benchPath, bytes.TrimSpace(got[idx:]), 0o644)
						return
					}
					want = bytes.TrimSpace(want)
					gotBench := bytes.TrimSpace(got[idx:])
					got = bytes.TrimSpace(got[:idx])
					if !bytes.Equal(got, want) {
						t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
					}
					if _, err := os.Stat(benchPath); err != nil {
						t.Fatalf("benchmark data missing")
					} else {
						data, err := os.ReadFile(benchPath)
						if err != nil || len(data) == 0 {
							t.Fatalf("benchmark data missing")
						}
						_ = gotBench // silence unused
					}
					return
				}
				if updateEnabled() {
					_ = os.WriteFile(outFile, got, 0o644)
				} else if !bytes.Equal(got, want) {
					t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
				}
				return
			}
			if updateEnabled() {
				_ = os.WriteFile(outFile, got, 0o644)
				return
			}
			if !bytes.Equal(got, want) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		})
	}
}

func updateSPOJReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "c")
	mdPath := filepath.Join(root, "transpiler", "x", "c", "SPOJ.md")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil || len(files) == 0 {
		return
	}
	sort.Slice(files, func(i, j int) bool {
		a, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		b, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return a < b
	})
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "| ---: | --- | :---: | ---: | ---: |")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := base
		if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".md"); err == nil {
			lines := strings.Split(string(data), "\n")
			if len(lines) > 0 {
				line := strings.TrimSpace(lines[0])
				line = strings.TrimPrefix(line, "#")
				line = strings.TrimSpace(line)
				if strings.HasPrefix(line, "[") {
					if r := strings.Index(line, "]"); r >= 0 {
						name = line[1:r]
					}
				} else if line != "" {
					name = line
				}
			}
		}
		status := ""
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = ""
		} else if _, err := os.Stat(filepath.Join(outDir, base+".c")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
			var r struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				data = data[idx:]
			}
			if json.Unmarshal(data, &r) == nil && r.Dur > 0 {
				dur = humanDur(time.Duration(r.Dur) * time.Microsecond)
				mem = humanSize(r.Mem)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 -0700")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# C SPOJ Transpiler Output\n\n")
	buf.WriteString("This directory stores C code generated from Mochi programs in `tests/spoj/x/mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-ctrans_update`.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
