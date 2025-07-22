//go:build slow

package ctrans_test

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

	ctrans "mochi/transpiler/x/c"
)

func TestRosettaTranspilerGolden(t *testing.T) {
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	var firstFail string
	for _, src := range files {
		if _, err := os.Stat(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			continue
		}
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		wantOut := filepath.Join(outDir, name+".out")
		t.Run(name, func(t *testing.T) {
			if firstFail != "" {
				t.Skip("skipping after first failure")
			}
			code, err := transpileFile(src)
			if err != nil {
				firstFail = name
				return
			}
			if updateEnabled() {
				norm := normalize(root, code)
				if err := os.WriteFile(filepath.Join(outDir, name+".c"), norm, 0o644); err != nil {
					t.Fatalf("write code: %v", err)
				}
			}
			got, runErr := transpileAndRun(src)
			if runErr != nil {
				if updateEnabled() {
					errPath := filepath.Join(outDir, name+".error")
					if werr := os.WriteFile(errPath, []byte(runErr.Error()+"\n"), 0o644); werr != nil {
						t.Fatalf("write error file: %v (run error: %v)", werr, runErr)
					}
				}
				firstFail = name
				return
			}
			if updateEnabled() {
				_ = os.Remove(filepath.Join(outDir, name+".error"))
				trimmed := bytes.TrimSpace(got)
				if err := os.WriteFile(wantOut, trimmed, 0o644); err != nil {
					t.Fatalf("write output: %v", err)
				}
			}
			trimmed := bytes.TrimSpace(got)
			wantData, err := os.ReadFile(wantOut)
			if err != nil {
				t.Fatalf("read expected: %v", err)
			}
			wantData = bytes.TrimSpace(wantData)
			if !bytes.Equal(trimmed, wantData) {
				t.Errorf("output mismatch for %s: got %q want %q", name, trimmed, wantData)
			}
		})
		if firstFail != "" {
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	readmePath := filepath.Join(root, "transpiler", "x", "c", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 -0700")
			}
		}
	}
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			mark = "[x]"
			compiled++
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# C Transpiler Rosetta Output\n\n")
	buf.WriteString("This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	buf.WriteString(fmt.Sprintf("Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
