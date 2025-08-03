//go:build slow

package lua_test

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
	lua "mochi/transpiler/x/lua"
	"mochi/types"
)

// shouldUpdate reports whether -update flag was provided.
func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

// rosettaIndex optionally selects a single program by 1-based index.
var rosettaIndex = flag.Int("index", 0, "run only the N-th Rosetta program (1-based)")

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

func runCase(src, outDir string) ([]byte, bool, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".lua")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, false, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, false, errs[0]
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	lua.SetBenchMain(bench)
	ast, err := lua.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, bench, err
	}
	code := lua.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, bench, err
	}
	cmd := exec.Command("lua", codePath)
	runEnv := append(os.Environ(), "MOCHI_NOW_SEED=1")
	if bench {
		// run with benchmarking while keeping deterministic seed
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
	}
	cmd.Env = runEnv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, bench, err
	}
	_ = os.Remove(errPath)
	out = bytes.TrimSpace(out)
	if bench {
		part := out
		if idx := bytes.LastIndexByte(out, '{'); idx >= 0 {
			part = out[idx:]
		}
		_ = os.WriteFile(benchPath, part, 0o644)
		return part, true, nil
	}
	return out, false, nil
}

func TestLuaTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Lua")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaReadme)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no Mochi Rosetta tests found")
	}
	var files []string
	for _, n := range names {
		files = append(files, filepath.Join(srcDir, n))
	}
	if *rosettaIndex > 0 {
		idx := *rosettaIndex
		if idx < 1 || idx > len(files) {
			t.Fatalf("invalid -index: %d", idx)
		}
		files = []string{files[idx-1]}
	} else if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		files = []string{filepath.Join(srcDir, only+".mochi")}
	} else if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	var passed int
	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			got, bench, err := runCase(src, outDir)
			if err != nil {
				t.Fatalf("run error: %v", err)
			}
			if bench {
				return
			}
			wantPath := filepath.Join(outDir, name+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				if shouldUpdate() {
					if werr := os.WriteFile(wantPath, append(got, '\n'), 0o644); werr == nil {
						t.Logf("updated: %s", wantPath)
						return
					} else {
						t.Fatalf("write golden: %v", werr)
					}
				}
				t.Fatalf("read golden: %v", err)
			}
			got = bytes.TrimSpace(got)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				if shouldUpdate() {
					if werr := os.WriteFile(wantPath, append(got, '\n'), 0o644); werr == nil {
						t.Logf("updated: %s", wantPath)
						return
					} else {
						t.Fatalf("write golden: %v", werr)
					}
				}
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
		if !ok {
			t.Fatalf("first failing program: %s", testName)
		}
		passed++
	}
	t.Logf("Summary: %d passed", passed)
}

func updateRosettaReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Lua")
	readmePath := filepath.Join(root, "transpiler", "x", "lua", "ROSETTA.md")

	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(srcDir, n)
	}
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".lua")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
		if data, err := os.ReadFile(benchFile); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &js) == nil {
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
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			loc := time.FixedZone("GMT+7", 7*3600)
			ts = t.In(loc).Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Lua Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Lua code for programs in `tests/rosetta/x/Mochi`. Each program has a `.lua` file produced by the transpiler and a `.out` file with its runtime output. Compilation or runtime errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "Transpiled programs: %d/%d\n\n", compiled, total)
	if ts != "" {
		buf.WriteString("Last updated: " + ts + "\n\n")
	}
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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
