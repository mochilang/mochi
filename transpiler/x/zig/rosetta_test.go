//go:build slow

package zigt_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	zigt "mochi/transpiler/x/zig"
	"mochi/types"
)

var updateRosetta = flag.Bool("update-rosetta-zig", false, "update rosetta golden files")

func TestZigTranspiler_Rosetta(t *testing.T) {
	t.Cleanup(updateRosettaReadme)
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	root := rosettaRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "out", "Zig")
	os.MkdirAll(outDir, 0o755)

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
		if err != nil {
			t.Fatalf("glob: %v", err)
		}
		sort.Strings(files)
		if idx > len(files) {
			t.Fatalf("index %d out of range (%d files)", idx, len(files))
		}
		src := files[idx-1]
		if _, err := runCase(src, outDir); err != nil {
			t.Fatalf("%s: %v", filepath.Base(src), err)
		}
		return
	}

	golden.RunFirstFailure(t, "tests/rosetta/x/Mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		return runCase(src, outDir)
	})
}

func runCase(src, outDir string) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(src), ".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("parse: %w", err))
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(outDir, name, fmt.Errorf("type: %v", errs[0]))
		return nil, errs[0]
	}
	ast, err := zigt.Transpile(prog, env)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("transpile: %v", err))
		return nil, err
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, name+".zig")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	cmd := exec.Command("zig", "run", codePath)
	if data, err := os.ReadFile(filepath.Join(filepath.Dir(src), name+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("run: %v\n%s", err, out))
		return nil, err
	}
	os.Remove(filepath.Join(outDir, name+".error"))
	outPath := filepath.Join(outDir, name+".out")
	if *updateRosetta {
		if err := os.WriteFile(outPath, append(got, '\n'), 0o644); err != nil {
			return nil, err
		}
	} else if want, err := os.ReadFile(outPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			return got, fmt.Errorf("output mismatch for %s", name)
		}
	}
	return got, nil
}

func writeErr(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func rosettaRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func updateRosettaReadme() {
	root := rosettaRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "out", "Zig")
	readme := filepath.Join(root, "transpiler", "x", "zig", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				mark = "[x]"
				compiled++
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
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
	var buf bytes.Buffer
	buf.WriteString("# Zig Rosetta Transpiler\n\n")
	buf.WriteString("Generated Zig code for Rosetta tasks lives under `tests/rosetta/out/Zig`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Program Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
