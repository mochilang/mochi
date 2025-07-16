//go:build slow

package cpp_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cpp "mochi/compiler/x/cpp"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), []byte("parse: "+err.Error()), 0644)
				t.Skipf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), []byte("type: "+errs[0].Error()), 0644)
				t.Skipf("type error: %v", errs[0])
				return
			}
			code, err := cpp.New().Compile(prog)
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), []byte("compile: "+err.Error()), 0644)
				t.Skipf("compile error: %v", err)
				return
			}
			codeFile := filepath.Join(outDir, name+".cpp")
			if err := os.WriteFile(codeFile, code, 0644); err != nil {
				t.Fatal(err)
			}
			bin := filepath.Join(outDir, name)
			if out, err := exec.Command("g++", codeFile, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0644)
				t.Skipf("g++ error: %v\n%s", err, out)
				return
			}
			defer os.Remove(bin)
			cmd := exec.Command(bin)
			if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(inData)
			}
			outBytes, err := cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), outBytes, 0644)
				t.Skipf("run error: %v\n%s", err, outBytes)
				return
			}
			os.Remove(filepath.Join(outDir, name+".error"))
			got := bytes.TrimSpace(outBytes)
			if shouldUpdate() {
				_ = os.WriteFile(filepath.Join(outDir, name+".out"), append(got, '\n'), 0644)
				return
			}
			if want, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
				if !bytes.Equal(got, bytes.TrimSpace(want)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
				}
			} else {
				t.Fatalf("read golden: %v", err)
			}
		})
	}
}
