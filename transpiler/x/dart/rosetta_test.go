//go:build slow

package dartt_test

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
	dartt "mochi/transpiler/x/dart"
	"mochi/types"
)

func TestDartTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Dart")
	os.MkdirAll(outDir, 0o755)

	files, _ := filepath.Glob(filepath.Join("tests", "rosetta", "x", "Mochi", "*.mochi"))
	sort.Strings(files)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			base := strings.TrimSuffix(filepath.Base(src), ".mochi")
			codePath := filepath.Join(outDir, base+".dart")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := dartt.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			var buf bytes.Buffer
			if err := dartt.Emit(&buf, ast); err != nil {
				_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
				t.Fatalf("emit: %v", err)
			}
			if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("dart", codePath)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			_ = os.WriteFile(outPath, got, 0o644)
		})
		if !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Dart")
	readmePath := filepath.Join(root, "transpiler", "x", "dart", "ROSETTA.md")

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
	completed := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				completed++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	buf.WriteString("# Dart Rosetta Transpiler Output\n\n")
	buf.WriteString("This directory contains Dart code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each program has a `.dart` file and `.out` output. Compilation or runtime failures are captured in a `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled and ran: %d/%d\n", completed, total)
	buf.WriteString("\n## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		buf.WriteString(fmt.Sprintf("\n_Last updated: %s_\n", ts))
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
