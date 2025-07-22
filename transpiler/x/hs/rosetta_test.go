//go:build slow

package hs_test

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
	hs "mochi/transpiler/x/hs"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func writeErr(root, name string, err error) {
	path := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell", name+".error")
	_ = os.WriteFile(path, []byte(err.Error()), 0o644)
}

func removeErr(root, name string) {
	os.Remove(filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell", name+".error"))
}

func runRosettaTask(t *testing.T, root, name string) {
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(root, name, fmt.Errorf("parse: %w", err))
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(root, name, fmt.Errorf("type: %v", errs[0]))
		t.Skipf("type error: %v", errs[0])
		return
	}
	hprog, err := hs.Transpile(prog, env)
	if err != nil {
		writeErr(root, name, fmt.Errorf("transpile: %w", err))
		t.Skipf("transpile error: %v", err)
		return
	}
	code := hs.Emit(hprog)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell")
	os.MkdirAll(outDir, 0o755)
	hsFile := filepath.Join(outDir, name+".hs")
	if err := os.WriteFile(hsFile, code, 0o644); err != nil {
		t.Fatalf("write hs: %v", err)
	}

	cmd := exec.Command("runhaskell", hsFile)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	outBytes, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(outBytes)
	if err != nil {
		writeErr(root, name, fmt.Errorf("run: %v\n%s", err, outBytes))
		t.Skipf("run error: %v", err)
		return
	}
	removeErr(root, name)
	outPath := filepath.Join(outDir, name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	if !bytes.Equal(got, bytes.TrimSpace(want)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
	}
}

func TestHSTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("runhaskell"); err != nil {
		t.Skip("runhaskell not installed")
	}
	root := repoRootDir(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	max := 3
	if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < len(files) {
			max = n
		} else if err == nil {
			max = n
		}
	}
	if len(files) < max {
		max = len(files)
	}
	firstErr := ""
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTask(t, root, name) })
		if firstErr == "" {
			if _, err := os.Stat(filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell", name+".error")); err == nil {
				firstErr = name
			}
		}
	}
	if firstErr != "" {
		t.Fatalf("first failing program: %s", firstErr)
	}
}

func updateChecklist() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell")
	readme := filepath.Join(root, "transpiler", "x", "hs", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Rosetta Haskell Transpiler (%d/%d succeeded)\n\n", compiled, total)
	buf.WriteString("Generated Haskell code for Rosetta Mochi programs. Each `.hs` file is in `tests/rosetta/transpiler/Haskell` with matching `.out` output. Failures produce a `.error` file.\n\n")
	out, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.In(loc).Format("2006-01-02 15:04 MST"))
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
