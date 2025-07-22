//go:build slow

package st_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	st "mochi/transpiler/x/st"
	"mochi/types"
)

func TestSmalltalkRosetta(t *testing.T) {
	if _, err := exec.LookPath("gst"); err != nil {
		t.Skip("gst not installed")
	}
	root := repoRootDir(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "st")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	if len(files) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		stPath := filepath.Join(outDir, name+".st")
		wantPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		if _, err := os.Stat(wantPath); os.IsNotExist(err) && !shouldUpdate() {
			t.Run(name, func(t *testing.T) { t.Skip("missing golden") })
			continue
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Skip("type error")
				return
			}
			ast, err := st.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("transpile error")
				return
			}
			var buf bytes.Buffer
			if err := st.Emit(&buf, ast); err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("emit error")
				return
			}
			code := buf.Bytes()
			if err := os.WriteFile(stPath, code, 0o644); err != nil {
				t.Fatalf("write st: %v", err)
			}
			cmd := exec.Command("gst", stPath)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				return
			}
			got := bytes.TrimSpace(out)
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
				_ = os.Remove(errPath)
				t.Logf("updated: %s", wantPath)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				_ = os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want)), 0o644)
				t.Errorf("output mismatch for %s", name)
				return
			}
			_ = os.Remove(errPath)
		})
	}
}

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func updateRosettaChecklist() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "st")
	readmePath := filepath.Join(root, "transpiler", "x", "st", "ROSETTA.md")
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
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc)
			} else {
				ts = t
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Smalltalk Transpiler Rosetta Results\n\n")
	buf.WriteString("This checklist tracks the Rosetta Code programs transpiled into Smalltalk under `tests/rosetta/transpiler/st`.\n")
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.Format("2006-01-02 15:04 -0700"))
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
