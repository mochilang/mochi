//go:build slow

package kt_test

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
	kt "mochi/transpiler/x/kt"
	"mochi/types"
)

func TestSpojKotlin(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "kotlin")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	idx := 1
	if s := os.Getenv("SPOJ_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v > 0 {
			idx = v
		}
	}
	srcPath := filepath.Join(srcDir, fmt.Sprintf("%d.mochi", idx))
	name := strconv.Itoa(idx)
	codePath := filepath.Join(outDir, name+".kt")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")
	benchPath := filepath.Join(outDir, name+".bench")
	inPath := filepath.Join(outDir, name+".in")

	bench := os.Getenv("MOCHI_BENCHMARK") == "true"

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
	kt.SetBenchMain(bench)
	ast, err := kt.Transpile(env, prog)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := kt.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	jar := filepath.Join(outDir, name+".jar")
	if out, err := exec.Command("kotlinc", codePath, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte("kotlinc: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("kotlinc: %v", err)
	}
	cmd := exec.Command("java", "-jar", jar)
	runEnv := append(os.Environ(), "MOCHI_ROOT="+root)
	if bench {
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
	} else {
		runEnv = append(runEnv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = runEnv
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(errPath, buf.Bytes(), 0o644)
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(buf.Bytes())
	if bench {
		_ = os.WriteFile(benchPath, got, 0o644)
	} else {
		want, err := os.ReadFile(outPath)
		if err != nil {
			t.Fatalf("read want: %v", err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			_ = os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want)), 0o644)
			t.Fatalf("output mismatch")
		}
	}
	_ = os.Remove(errPath)
	_ = os.Remove(jar)

	t.Cleanup(updateSPOJ)
}

func updateSPOJ() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "kotlin")
	md := filepath.Join(root, "transpiler", "x", "kt", "SPOJ.md")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return
	}
	sort.Strings(files)
	total := len(files)
	compiled := 0
	rows := []string{"| Index | Name | Status | Duration | Memory |", "| ---: | --- | :---: | ---: | ---: |"}
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := base
		if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".md"); err == nil {
			if line, _, ok := bytes.Cut(data, []byte("\n")); ok || len(line) > 0 {
				line = bytes.TrimSpace(line)
				if i := bytes.IndexByte(line, '['); i >= 0 {
					if j := bytes.IndexByte(line[i+1:], ']'); j >= 0 {
						name = string(line[i+1 : i+1+j])
					}
				} else if len(line) > 0 && line[0] == '#' {
					name = strings.TrimSpace(string(line[1:]))
				}
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, base+".kt")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				var r struct {
					Dur int64 `json:"duration_us"`
					Mem int64 `json:"memory_bytes"`
				}
				if json.Unmarshal(data[idx:], &r) == nil {
					dur = formatDuration(time.Duration(r.Dur) * time.Microsecond)
					mem = formatBytes(r.Mem)
				}
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, base+".out")); err == nil {
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				var r struct {
					Dur int64 `json:"duration_us"`
					Mem int64 `json:"memory_bytes"`
				}
				if json.Unmarshal(data[idx:], &r) == nil {
					dur = formatDuration(time.Duration(r.Dur) * time.Microsecond)
					mem = formatBytes(r.Mem)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*60*60)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Kotlin code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/kotlin`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
