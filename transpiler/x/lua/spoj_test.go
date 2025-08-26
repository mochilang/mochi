//go:build slow

package lua_test

import (
	"bytes"
	"encoding/json"
	"flag"
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
	lua "mochi/transpiler/x/lua"
	"mochi/types"
)

var updateSpojFlag = flag.Bool("update-spoj-lua", false, "update golden files")

func spojUpdateEnabled() bool { return *updateSpojFlag }

// readProblemName extracts problem title from the accompanying .md file.
func readProblemName(dir, idx string) string {
	mdPath := filepath.Join(dir, idx+".md")
	data, err := os.ReadFile(mdPath)
	if err != nil {
		return idx
	}
	line := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
	if strings.HasPrefix(line, "#") {
		line = strings.TrimSpace(strings.TrimPrefix(line, "#"))
		if i := strings.Index(line, "]"); i > 0 {
			if j := strings.Index(line, "["); j >= 0 && j < i {
				return line[j+1 : i]
			}
		}
		return line
	}
	return idx
}

func TestLuaTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "lua")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpojChecklist)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("list mochi files: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}
	sort.Strings(files)
	if v := os.Getenv("MOCHI_SPOJ_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(files) {
			files = files[n-1 : n]
		} else {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", v)
		}
	}

	for _, src := range files {
		idx := strings.TrimSuffix(filepath.Base(src), ".mochi")
		name := readProblemName(srcDir, idx)
		testName := fmt.Sprintf("%03s_%s", idx, strings.ReplaceAll(name, " ", "_"))
		t.Run(testName, func(t *testing.T) {
			runSpojCase(t, srcDir, outDir, idx)
		})
	}
}

func runSpojCase(t *testing.T, srcDir, outDir, idx string) {
	t.Helper()
	srcPath := filepath.Join(srcDir, idx+".mochi")
	codePath := filepath.Join(outDir, idx+".lua")
	outPath := filepath.Join(outDir, idx+".out")
	errPath := filepath.Join(outDir, idx+".error")
	benchPath := filepath.Join(outDir, idx+".bench")
	inPath := filepath.Join(outDir, idx+".in")

	bench := true
	want, err := os.ReadFile(outPath)
	if err != nil {
		if !spojUpdateEnabled() {
			t.Fatalf("read want: %v", err)
		}
	}

	prog, err := parser.Parse(srcPath)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	lua.SetBenchMain(bench)
	ast, err := lua.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := lua.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("lua", codePath)
	runEnv := append(os.Environ(), "MOCHI_NOW_SEED=1")
	if bench {
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
	}
	cmd.Env = runEnv
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		if spojUpdateEnabled() {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return
		}
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	benchBytes := []byte{}
	if bench {
		if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
			benchBytes = bytes.TrimSpace(got[idx:])
			got = bytes.TrimSpace(got[:idx])
		}
	}
	if spojUpdateEnabled() {
		if len(benchBytes) > 0 {
			_ = os.WriteFile(benchPath, benchBytes, 0o644)
		}
		_ = os.WriteFile(outPath, got, 0o644)
		return
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", idx, got, want)
	}
}

func updateSpojChecklist() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "lua")
	mdPath := filepath.Join(root, "transpiler", "x", "lua", "SPOJ.md")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return
	}
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, f := range files {
		idx := strings.TrimSuffix(filepath.Base(f), ".mochi")
		name := readProblemName(srcDir, idx)
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, idx+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, idx+".lua")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, idx+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				if r.DurationUS > 0 {
					dur = humanDuration(r.DurationUS)
				}
				if r.MemoryBytes > 0 {
					mem = humanSize(r.MemoryBytes)
				}
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, idx+".out")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if pos := bytes.LastIndexByte(data, '{'); pos >= 0 {
				if json.Unmarshal(data[pos:], &r) == nil && r.DurationUS > 0 {
					dur = humanDuration(r.DurationUS)
					mem = humanSize(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Lua code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/lua`.\n")
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
