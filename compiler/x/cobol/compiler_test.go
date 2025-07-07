package cobol_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

// compileFile compiles src to COBOL and returns the path to the generated file.
func compileFile(t *testing.T, src string, outDir string) (string, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", fmt.Errorf("type error: %v", errs[0])
	}
	c := cobol.New(env, src)
	code, err := c.Compile(prog)
	if err != nil {
		return "", err
	}
	dst := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(src), ".mochi")+".cob")
	if err := os.WriteFile(dst, code, 0644); err != nil {
		return "", err
	}
	return dst, nil
}

func TestCompileAllValid(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	rootDir := filepath.Join(repoRoot(t), "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(rootDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(repoRoot(t), "tests", "machine", "x", "cobol")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			os.Remove(filepath.Join(outDir, name+".out"))
			os.Remove(filepath.Join(outDir, name+".error"))
			cobFile, err := compileFile(t, src, outDir)
			if err != nil {
				writeError(src, outDir, name, err)
				return
			}
			exe := filepath.Join(outDir, name)
			if out, err := exec.Command("cobc", "-free", "-x", cobFile, "-o", exe).CombinedOutput(); err != nil {
				writeError(src, outDir, name, fmt.Errorf("cobc error: %v\n%s", err, out))
				return
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				writeError(src, outDir, name, fmt.Errorf("run error: %v\n%s", err, out))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
			os.Remove(exe)
		})
	}
}

func writeError(src, outDir, name string, err error) {
	data, _ := os.ReadFile(src)
	msg := fmt.Sprintf("error: %v\n\nsource:\n%s", err, data)
	os.WriteFile(filepath.Join(outDir, name+".error"), []byte(msg), 0644)
}
