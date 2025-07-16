package kotlin_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	kotlin "mochi/compiler/x/kotlin"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func stripHeader(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("// Generated")) {
		return b[i+1:]
	}
	return b
}

func repoRoot() string {
	dir, _ := os.Getwd()
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
	return dir
}

func compileRun(t *testing.T, src string) ([]byte, error) {
	root := repoRoot()
	outDir := filepath.Join(root, "tests", "machine", "x", "kotlin")
	os.MkdirAll(outDir, 0o755)
	name := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, name+".kt")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, errs[0]
	}
	code, err := kotlin.New(env, filepath.Base(src)).Compile(prog)
	if err != nil {
		os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}

	tmp := t.TempDir()
	ktFile := filepath.Join(tmp, "Main.kt")
	if err := os.WriteFile(ktFile, code, 0o644); err != nil {
		return nil, err
	}
	jar := filepath.Join(tmp, "main.jar")
	if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		os.WriteFile(errPath, out, 0o644)
		return nil, err
	}
	cmd := exec.Command("java", "-jar", jar)
	cmd.Dir = filepath.Dir(filepath.Dir(src))
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	outBytes, err := cmd.CombinedOutput()
	if err != nil {
		os.WriteFile(errPath, outBytes, 0o644)
		return nil, err
	}
	os.Remove(errPath)
	os.WriteFile(outPath, bytes.TrimSpace(outBytes), 0o644)
	return bytes.TrimSpace(outBytes), nil
}

func compileSource(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	code, err := kotlin.New(env, filepath.Base(src)).Compile(prog)
	if err != nil {
		return nil, err
	}
	return bytes.TrimSpace(stripHeader(code)), nil
}

func TestKotlinCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		return compileRun(t, src)
	})
	golden.Run(t, "tests/vm/valid", ".mochi", ".kt.out", func(src string) ([]byte, error) {
		return compileSource(src)
	})
}
