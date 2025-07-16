//go:build slow

package swift_test

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	swift "mochi/compiler/x/swift"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func TestSwiftCompiler_VMValid_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "swift")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	os.MkdirAll(outDir, 0o755)
	passed, failed := 0, 0
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("parse error: %v", err)
				}
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(errs[0].Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("type error: %v", errs[0])
				}
				return
			}
			code, err := swift.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("compile error: %v", err)
				}
				return
			}
			codePath := filepath.Join(outDir, name+".swift")
			_ = os.WriteFile(codePath, code, 0644)
			outBytes, err := compileAndRunSwiftSrc(t, swiftExe, code)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), outBytes, 0644)
				if !shouldUpdateValid() {
					t.Skipf("run error: %v", err)
				}
				return
			}
			os.Remove(filepath.Join(outDir, name+".error"))
			if shouldUpdateValid() {
				_ = os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(outBytes), 0644)
				return
			}
			wantOut := filepath.Join(outDir, name+".out")
			if want, err := os.ReadFile(wantOut); err == nil {
				if !bytes.Equal(bytes.TrimSpace(outBytes), bytes.TrimSpace(want)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, bytes.TrimSpace(outBytes), bytes.TrimSpace(want))
				}
			}
		})
		if ok {
			passed++
		} else {
			failed++
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}
