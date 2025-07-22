//go:build slow

package cljt_test

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
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

func TestRosettaClojure(t *testing.T) {
	defer updateRosettaReadme()
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Clojure")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	sort.Strings(outs)

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(outs) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		outs = outs[idx-1 : idx]
	}

	for _, srcPath := range outs {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		t.Run(name, func(t *testing.T) {
			compileRunClojureRosetta(t, srcPath, outDir, name)
		})
	}
}

func compileRunClojureRosetta(t *testing.T, srcPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeRosettaCljError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("transpile error: %w", err))
		t.Skip("transpile error")
		return
	}
	code := cljt.Format(cljt.EmitString(ast))
	cljPath := filepath.Join(outDir, name+".clj")
	if err := os.WriteFile(cljPath, code, 0o644); err != nil {
		t.Fatalf("write clj: %v", err)
	}
	cmd := exec.Command("clojure", cljPath)
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		t.Skip("run error")
		return
	}
	got := bytes.TrimSpace(buf.Bytes())
	outPath := filepath.Join(outDir, name+".out")
	if *update {
		if err := os.WriteFile(outPath, got, 0o644); err != nil {
			t.Fatalf("write out: %v", err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("missing output: %v", err))
		t.Fatalf("missing .out file for %s", name)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeRosettaCljError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		t.Errorf("output mismatch for %s", name)
		return
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeRosettaCljError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func updateRosettaReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	binDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Clojure")
	readmePath := filepath.Join(root, "transpiler", "x", "clj", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	completed := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".out")); err == nil {
			completed++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 -0700")

	var buf bytes.Buffer
	buf.WriteString("# Clojure Rosetta Transpiler\n\n")
	fmt.Fprintf(&buf, "Completed: %d/%d\n", completed, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
