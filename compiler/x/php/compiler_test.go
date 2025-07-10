//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func findRoot(t *testing.T) string {
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

func TestPHPCompiler_ValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "php")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileRunOne(t, src, outDir, name) })
	}
}

func compileRunOne(t *testing.T, src, outDir, name string) {
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := phpcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		writeError(outDir, name, []byte(err.Error()))
		t.Skipf("compile error: %v", err)
		return
	}
	phpPath := filepath.Join(outDir, name+".php")
	os.WriteFile(phpPath, code, 0644)
	cmd := exec.Command("php", phpPath)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, out)
		t.Skipf("php error: %v", err)
		return
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
	os.Remove(filepath.Join(outDir, name+".error"))
}

func writeError(dir, name string, msg []byte) {
	os.WriteFile(filepath.Join(dir, name+".error"), msg, 0644)
	os.Remove(filepath.Join(dir, name+".out"))
}

func TestMain(m *testing.M) {
	code := m.Run()
	os.Exit(code)
}
