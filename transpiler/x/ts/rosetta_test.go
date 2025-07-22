//go:build slow

package tstranspiler_test

import (
	"bytes"
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
	meta "mochi/transpiler/meta"
	ts "mochi/transpiler/x/ts"
	"mochi/types"
)

func TestTSTranspiler_Rosetta_Golden(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "TypeScript")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	max := 3
	if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < max {
			max = n
		}
	}
	if len(files) < max {
		max = len(files)
	}
	for _, src := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTask(t, srcDir, outDir, name) })
	}
}

func runRosettaTask(t *testing.T, srcDir, outDir, name string) {
	src := filepath.Join(srcDir, name+".mochi")
	codePath := filepath.Join(outDir, name+".ts")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}
	tsProg, err := ts.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}
	code := ts.Emit(tsProg)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", codePath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	if data, err := os.ReadFile(filepath.Join(srcDir, name+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), buf.Bytes()...), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(errPath, []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(outPath, []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(errPath)
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "TypeScript")
	readmePath := filepath.Join(root, "transpiler", "x", "ts", "ROSETTA.md")

	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	passed := 0
	var lines []string
	for _, f := range files {
		name := filepath.Base(f)
		base := strings.TrimSuffix(name, ".mochi")
		mark := "[ ]"
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")
		goldPath := filepath.Join(srcDir, base+".out")
		if _, err := os.Stat(outPath); err == nil {
			if _, err2 := os.Stat(errPath); err2 == nil {
				// failed execution
			} else if want, err2 := os.ReadFile(goldPath); err2 == nil {
				if got, err3 := os.ReadFile(outPath); err3 == nil {
					if bytes.Equal(bytes.TrimSpace(got), bytes.TrimSpace(want)) {
						passed++
						mark = "[x]"
					}
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, base))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi \u2192 TypeScript Rosetta Transpiler\n\n")
	buf.WriteString("Generated TypeScript for the Mochi Rosetta suite lives under `tests/rosetta/transpiler/TypeScript`.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", passed, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		fmt.Fprintf(&buf, "\n_Last updated: %s_\n", ts)
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
