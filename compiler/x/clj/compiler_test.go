//go:build slow

package cljcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cljcode "mochi/compiler/x/clj"
	"mochi/parser"
	"mochi/types"
)

// compileAndRun compiles src file to Clojure, writes output or error.
func compileAndRun(t *testing.T, root, src string) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	outDir := filepath.Join(root, "tests", "machine", "x", "clj")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	codePath := filepath.Join(outDir, base+".clj")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		writeError(errPath, err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(errPath, errs[0])
		return
	}
	c := cljcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		writeError(errPath, err)
		return
	}
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("clojure", codePath)
	inputPath := strings.TrimSuffix(src, ".mochi") + ".in"
	if data, err := os.ReadFile(inputPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(errPath, fmt.Errorf("%v\n%s", err, out))
		return
	}
	if err := os.WriteFile(outPath, out, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	os.Remove(errPath)
}

func writeError(path string, err error) {
	msg := err.Error()
	type formatter interface{ Format() string }
	if f, ok := err.(formatter); ok {
		msg = f.Format()
	}
	_ = os.WriteFile(path, []byte(msg), 0o644)
}

func findRepoRoot(t *testing.T) string {
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "clj")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Clojure Machine Translations\n\n")
	buf.WriteString("This directory contains Clojure code generated from the Mochi programs in `tests/vm/valid` using the Clojure compiler. Each program was compiled and executed. Successful runs produced an `.out` file while failures produced an `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d successful.\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n## Status\n")
	if compiled == total {
		buf.WriteString("All example programs compile and run successfully.\n")
	} else {
		fmt.Fprintf(&buf, "%d programs failed.\n", total-compiled)
	}
	buf.WriteString("\n## Remaining tasks\n")
	if compiled == total {
		buf.WriteString("None\n")
	} else {
		buf.WriteString("- [ ] Fix failing examples\n")
	}
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}
