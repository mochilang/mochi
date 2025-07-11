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

func TestCompileValidPrograms(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, f := range files {
		f := f
		t.Run(filepath.Base(f), func(t *testing.T) {
			compileAndRun(t, root, f)
		})
	}
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
	os.Exit(code)
}

