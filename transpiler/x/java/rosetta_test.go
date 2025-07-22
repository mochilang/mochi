//go:build slow

package javatr_test

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
	javatr "mochi/transpiler/x/java"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func runRosettaTask(t *testing.T, name string) {
	root := repoRootDir(t)
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	codePath := filepath.Join(outDir, name+".java")
	wantOut := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0644)
		t.Fatalf("parse %s: %v", name, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0644)
		t.Fatalf("type %s: %v", name, errs[0])
	}
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0644)
		t.Fatalf("transpile %s: %v", name, err)
	}
	code := javatr.Emit(ast)
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		t.Fatalf("write code %s: %v", name, err)
	}

	className := "Main"
	tmp := t.TempDir()
	srcTmp := filepath.Join(tmp, className+".java")
	if err := os.WriteFile(srcTmp, code, 0644); err != nil {
		t.Fatalf("tmp write %s: %v", name, err)
	}
	cmd := exec.Command("javac", "-d", tmp, srcTmp)
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0644)
		t.Fatalf("javac %s: %v", name, err)
	}
	cmd = exec.Command("java", "-cp", tmp, className)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err = cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0644)
		t.Fatalf("run %s: %v", name, err)
	}
	_ = os.Remove(errPath)
	if shouldUpdateRosetta() {
		_ = os.WriteFile(wantOut, append(got, '\n'), 0644)
	} else if want, err := os.ReadFile(wantOut); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}
}

func TestJavaTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRootDir(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)

	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		idx, err := strconv.Atoi(v)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid ROSETTA_INDEX %s", v)
		}
		files = files[idx-1 : idx]
	} else if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < len(files) {
			files = files[:n]
		}
	}

	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	os.MkdirAll(outDir, 0o755)
	var firstFail string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		ok := t.Run(name, func(t *testing.T) { runRosettaTask(t, name) })
		if !ok {
			firstFail = name
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateRosetta() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	readmePath := filepath.Join(root, "transpiler", "x", "java", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
	ts := time.Now().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Java Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Java code for programs in `tests/rosetta/x/Mochi`. Each program has a `.java` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Checklist (%d/%d) - updated %s\n", compiled, total, ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0644)
}
