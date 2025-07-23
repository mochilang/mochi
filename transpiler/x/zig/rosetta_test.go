//go:build slow

package zigt_test

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	zigt "mochi/transpiler/x/zig"
	"mochi/types"
)

var updateRosetta = flag.Bool("update-rosetta-zig", false, "update rosetta golden files")

func readIndex(dir string) ([]string, error) {
	path := filepath.Join(dir, "index.txt")
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var names []string
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		if len(fields) == 2 {
			names = append(names, fields[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func TestZigTranspiler_Rosetta(t *testing.T) {
	t.Cleanup(updateRosettaReadme)
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	root := rosettaRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Zig")
	os.MkdirAll(outDir, 0o755)

	names, err := readIndex(srcDir)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		name := names[idx-1]
		src := filepath.Join(srcDir, name)
		if _, err := runCase(src, outDir); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		return
	}

	var firstFail string
	for _, name := range names {
		src := filepath.Join(srcDir, name)
		base := strings.TrimSuffix(name, ".mochi")
		ok := t.Run(base, func(t *testing.T) {
			if _, err := runCase(src, outDir); err != nil {
				t.Fatalf("%v", err)
			}
		})
		if !ok {
			firstFail = base
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
	return
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
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=0")
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
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Zig")
	readme := filepath.Join(root, "transpiler", "x", "zig", "ROSETTA.md")

	names, err := readIndex(srcDir)
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	for i, name := range names {
		base := strings.TrimSuffix(name, ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, base+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, base+".error")); os.IsNotExist(err2) {
				mark = "[x]"
				compiled++
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, base))
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
	buf.WriteString("Generated Zig code for Rosetta tasks lives under `tests/rosetta/transpiler/Zig`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Program Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
