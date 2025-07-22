//go:build slow

package fstrans_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func ensureFSharp(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}
}

func repoRootRosetta(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestFSTranspiler_Rosetta_Golden(t *testing.T) {
	ensureFSharp(t)
	root := repoRootRosetta(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "FS")
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaReadme)

	pattern := filepath.Join(srcDir, "*.mochi")
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		pattern = filepath.Join(srcDir, only+".mochi")
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no Mochi Rosetta tests found: %s", pattern)
	}
	sort.Strings(files)

	var passed, failed int
	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".fs")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")
			exePath := filepath.Join(outDir, base+".exe")

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
			ast, err := fstrans.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile error: %v", err)
			}
			code := fstrans.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exePath, codePath)
			if out, err := cmd.CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			run := exec.Command("mono", exePath)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				run.Stdin = bytes.NewReader(data)
			}
			out, err := run.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			if shouldUpdate := os.Getenv("UPDATE"); shouldUpdate == "1" || shouldUpdate == "true" {
				_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
				return
			}
			_ = os.WriteFile(outPath, got, 0o644)
			if want, err := os.ReadFile(outPath); err == nil {
				want = bytes.TrimSpace(want)
				if !bytes.Equal(got, want) {
					t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
				}
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = base
			}
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

// updateRosettaReadme is registered via t.Cleanup to regenerate ROSETTA.md.
func updateRosettaReadme() { updateRosetta() }

func updateRosetta() {
	root := repoRootRosetta(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "FS")
	readmePath := filepath.Join(root, "transpiler", "x", "fs", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				mark = "[x]"
				compiled++
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		ts = t.Format("2006-01-02 15:04 -0700")
	}

	var buf bytes.Buffer
	buf.WriteString("# F# Rosetta Transpiler\n\n")
	buf.WriteString("This file is auto-generated from rosetta tests.\n\n")
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n")
	fmt.Fprintf(&buf, "Last updated: %s\n", ts)
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
