//go:build slow

package kotlin_test

import (
	"bytes"
	"fmt"
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

// TestKotlinCompiler_VMValid_Golden compiles the Mochi programs in tests/vm/valid
// to Kotlin, runs them with the JVM and compares the runtime output to the
// golden .out files. Generated source and outputs are written under
// tests/machine/x/kotlin.
func TestKotlinCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	root := repoRoot()
	outDir := filepath.Join(root, "tests", "machine", "x", "kotlin")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		c := kotlin.New(env, filepath.Base(src))
		code, err := c.Compile(prog)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("compile: "+err.Error()), 0o644)
			return nil, fmt.Errorf("compile error: %w", err)
		}
		ktFile := filepath.Join(outDir, base+".kt")
		if err := os.WriteFile(ktFile, code, 0o644); err != nil {
			return nil, err
		}
		jar := filepath.Join(outDir, base+".jar")
		if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), append([]byte("kotlinc: "+err.Error()+"\n"), out...), 0o644)
			return nil, fmt.Errorf("kotlinc error: %w", err)
		}
		cmd := exec.Command("java", "-jar", jar)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
			return nil, fmt.Errorf("run error: %w", err)
		}
		out = bytes.TrimSpace(out)
		_ = os.WriteFile(filepath.Join(outDir, base+".out"), out, 0o644)
		_ = os.Remove(jar)
		_ = os.Remove(filepath.Join(outDir, base+".error"))
		return out, nil
	})
}
