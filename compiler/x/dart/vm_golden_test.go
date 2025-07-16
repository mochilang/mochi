package dart_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	dart "mochi/compiler/x/dart"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func stripHeader(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("// Generated")) {
		return b[i+1:]
	}
	return b
}

func TestDartCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}

	compileRun := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := dart.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.dart")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("dart", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	}

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", compileRun)

	golden.Run(t, "tests/vm/valid", ".mochi", ".dart.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := dart.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		return bytes.TrimSpace(stripHeader(code)), nil
	})
}
