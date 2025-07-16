//go:build slow

package ocaml_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ocaml "mochi/compiler/x/ocaml"
	testutil "mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func goldenShouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestOCamlCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "ocaml")
	os.MkdirAll(outDir, 0755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := ocaml.New(env).Compile(prog, src)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		mlFile := filepath.Join(outDir, base+".ml")
		if err := os.WriteFile(mlFile, code, 0644); err != nil {
			return nil, fmt.Errorf("write ml: %w", err)
		}
		exe := filepath.Join(outDir, base)
		if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
			os.WriteFile(filepath.Join(outDir, base+".error"), out, 0644)
			return nil, fmt.Errorf("ocamlc error: %v", err)
		}
		cmd := exec.Command(exe)
		var out bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &out
		if err := cmd.Run(); err != nil {
			os.WriteFile(filepath.Join(outDir, base+".error"), out.Bytes(), 0644)
			return nil, fmt.Errorf("run error: %v", err)
		}
		os.Remove(filepath.Join(outDir, base+".error"))
		os.WriteFile(filepath.Join(outDir, base+".out"), out.Bytes(), 0644)
		os.Remove(exe)
		os.Remove(mlFile[:len(mlFile)-3] + ".cmi")
		os.Remove(mlFile[:len(mlFile)-3] + ".cmo")
		if goldenShouldUpdate() {
			_ = os.WriteFile(strings.TrimSuffix(src, ".mochi")+".ml.out", code, 0644)
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}
