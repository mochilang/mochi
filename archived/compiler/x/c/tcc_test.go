//go:build archived && slow && tcc && libtcc

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/archived/x/c"
	"mochi/golden"
	"mochi/parser"
	"mochi/tools/tcc"
	"mochi/types"
)

// TestTinyCCPrograms compiles example programs with the C backend and
// executes them using TinyCC. It mirrors the regular C compiler tests
// but uses the embedded TinyCC library instead of the system toolchain.
func TestTinyCCPrograms(t *testing.T) {
	if err := tcc.EnsureTCC(); err != nil {
		t.Skipf("TinyCC not installed: %v", err)
	}

	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		code, err := ccode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		exe := filepath.Join(dir, "prog")
		if err := tcc.CompileToFile(string(code), exe); err != nil {
			return nil, fmt.Errorf("\u274c tcc error: %w", err)
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", run)
	golden.Run(t, "tests/compiler/c", ".mochi", ".out", run)
}
