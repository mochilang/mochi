//go:build slow

package fstrans_test

import (
	"bufio"
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

	_ = updateIndex(srcDir)
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	var files []string
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		files = append(files, filepath.Join(srcDir, only+".mochi"))
	} else {
		for _, n := range names {
			files = append(files, filepath.Join(srcDir, n))
		}
	}
	if len(files) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = files[idx-1 : idx]
	}

	var passed, failed int
	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		safe := strings.ReplaceAll(base, "+", "_")
		ok := t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, safe+".fs")
			outPath := filepath.Join(outDir, safe+".out")
			errPath := filepath.Join(outDir, safe+".error")
			exePath := filepath.Join(outDir, safe+".exe")

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

	idxPath := filepath.Join(srcDir, "index.txt")
	data, _ := os.ReadFile(idxPath)
	var files []string
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		fields := strings.Fields(line)
		if len(fields) >= 2 {
			files = append(files, strings.TrimSuffix(fields[1], ".mochi"))
		}
	}
	total := len(files)
	compiled := 0
	var lines []string
	for i, name := range files {
		mark := "[ ]"
		safe := strings.ReplaceAll(name, "+", "_")
		if _, err := os.Stat(filepath.Join(outDir, safe+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, safe+".error")); os.IsNotExist(err2) {
				mark = "[x]"
				compiled++
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s (index %d)", i+1, mark, name, i+1))
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

func updateIndex(dir string) error {
	pattern := filepath.Join(dir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		return err
	}
	sort.Strings(files)
	var buf bytes.Buffer
	for i, f := range files {
		fmt.Fprintf(&buf, "%d %s\n", i+1, filepath.Base(f))
	}
	return os.WriteFile(filepath.Join(dir, "index.txt"), buf.Bytes(), 0o644)
}
