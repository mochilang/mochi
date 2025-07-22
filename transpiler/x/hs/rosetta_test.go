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

func runRosettaTask(root, name string) error {
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(root, name, fmt.Errorf("parse: %w", err))
		return fmt.Errorf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(root, name, fmt.Errorf("type: %v", errs[0]))
		return fmt.Errorf("type error: %v", errs[0])
	}
	hprog, err := hs.Transpile(prog, env)
	if err != nil {
		writeErr(root, name, fmt.Errorf("transpile: %w", err))
		return fmt.Errorf("transpile error: %v", err)
	}
	code := hs.Emit(hprog)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell")
	os.MkdirAll(outDir, 0o755)
	hsFile := filepath.Join(outDir, name+".hs")
	if err := os.WriteFile(hsFile, code, 0o644); err != nil {
		return fmt.Errorf("write hs: %v", err)
	}

	cmd := exec.Command("runhaskell", hsFile)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	outBytes, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(outBytes)
	if err != nil {
		writeErr(root, name, fmt.Errorf("run: %v\n%s", err, outBytes))
		return fmt.Errorf("run error: %v", err)
	}
	removeErr(root, name)
	outPath := filepath.Join(outDir, name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return nil
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		return fmt.Errorf("read want: %v", err)
	}
	if !bytes.Equal(got, bytes.TrimSpace(want)) {
		return fmt.Errorf("output mismatch for %s.out", name)
	}
	return nil
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

	if idxStr := os.Getenv("ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx <= 0 || idx > len(files) {
			t.Fatalf("invalid ROSETTA_INDEX %s", idxStr)
		}
		files = files[idx-1 : idx]
	} else {
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
		files = files[:max]
	}

	var firstErr string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			if err := runRosettaTask(root, name); err != nil {
				firstErr = name
				t.Fatalf("%v", err)
			}
		})
		if !ok {
			break
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
