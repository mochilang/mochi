//go:build slow

package scalat_test

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

	"mochi/compiler/x/testutil"
	"mochi/parser"
	scalat "mochi/transpiler/x/scala"
	"mochi/types"
)

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		if len(fields) == 2 {
			names = append(names, fields[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func TestScalaTranspiler_Rosetta_Golden(t *testing.T) {
	ensureScala(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Scala")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaChecklist)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no programs found in index")
	}

	start := 0
	if v := os.Getenv("MOCHI_ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(names) {
			start = n - 1
			names = names[start : start+1]
		}
	} else {
		names = names[:1]
	}

	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	scalat.SetBenchMain(bench)

	var passed, failed int
	for i, nameFile := range names {
		name := strings.TrimSuffix(filepath.Base(nameFile), ".mochi")
		ok := t.Run(fmt.Sprintf("%03d_%s", start+i+1, name), func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".scala")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")
			benchPath := filepath.Join(outDir, name+".bench")

			prog, err := parser.Parse(filepath.Join(srcDir, nameFile))
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := scalat.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := scalat.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			tmp := t.TempDir()
			if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			cmd := exec.Command("scala", "-cp", tmp, "Main")
			cmdEnv := append([]string{}, os.Environ()...)
			if !bench {
				cmdEnv = append(cmdEnv, "MOCHI_NOW_SEED=1")
			}
			cmd.Env = cmdEnv
			if data, err := os.ReadFile(strings.TrimSuffix(filepath.Join(srcDir, nameFile), ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(outPath)
			if !bench {
				want = bytes.TrimSpace(want)
				if idx := bytes.LastIndexByte(want, '{'); idx >= 0 && bytes.Contains(want[idx:], []byte("duration_us")) {
					want = bytes.TrimSpace(want[:idx])
				}
			}

			outBytes, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(outBytes)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), outBytes...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			if bench {
				if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
					got = got[idx:]
				}
				_ = os.WriteFile(benchPath, got, 0o644)
				return
			}
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 && bytes.Contains(got[idx:], []byte("duration_us")) {
				got = bytes.TrimSpace(got[:idx])
			}
			_ = os.WriteFile(outPath, got, 0o644)

			if !updating() && len(want) > 0 {
				if !bytes.Equal(got, want) {
					t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
				}
			}
		})
		if ok {
			passed++
		} else {
			failed++
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}

func updateRosettaChecklist() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Scala")
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
		name := strings.TrimSuffix(filepath.Base(nameFile), ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".scala")); err == nil {
			compiled++
			status = "✓"
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil {
				if js.Duration > 0 {
					dur = humanDuration(js.Duration)
				}
				if js.Memory > 0 {
					mem = humanSize(js.Memory)
				}
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &js) == nil && js.Duration > 0 {
					dur = humanDuration(js.Duration)
					mem = humanSize(js.Memory)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 -0700")
	var buf bytes.Buffer
	buf.WriteString("# Scala Transpiler Rosetta Output\n\n")
	buf.WriteString("Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "## Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(fmt.Sprintf("_Last updated: %s_\n\n", ts))
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "transpiler", "x", "scala", "ROSETTA.md"), buf.Bytes(), 0o644)
}

func humanDuration(us int64) string {
	d := time.Duration(us) * time.Microsecond
	return d.String()
}

func humanSize(b int64) string {
	const unit = 1024
	if b < unit {
		return fmt.Sprintf("%d B", b)
	}
	div, exp := int64(unit), 0
	for n := b / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(b)/float64(div), "KMGTPE"[exp])
}

func updating() bool {
	f := flag.Lookup("update")
	if f == nil {
		return false
	}
	if getter, ok := f.Value.(interface{ Get() any }); ok {
		if v, ok2 := getter.Get().(bool); ok2 {
			return v
		}
	}
	return false
}
