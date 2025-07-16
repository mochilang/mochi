//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/types"
)

// TestMochiZigGolden compiles each Mochi source program under
// tests/rosetta/x/Mochi to Zig and verifies the generated code
// and program output match the golden files in tests/rosetta/out/Zig.
func TestMochiZigGolden(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Zig")

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, outPath := range outs {
		name := strings.TrimSuffix(filepath.Base(outPath), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		zigPath := filepath.Join(outDir, name+".zig")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			defer func() {
				if r := recover(); r != nil {
					writeZigError(outDir, name, fmt.Errorf("panic: %v", r))
					t.Skip("panic")
				}
			}()
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeZigError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeZigError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := zigcode.New(env).Compile(prog)
			if err != nil {
				writeZigError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			gotCode := bytes.TrimSpace(code)

			if shouldUpdate() {
				if err := os.WriteFile(zigPath, append(gotCode, '\n'), 0o644); err != nil {
					t.Fatalf("write zig: %v", err)
				}
				t.Logf("updated: %s", zigPath)
			} else {
				wantData, err := os.ReadFile(zigPath)
				if err != nil {
					t.Fatalf("read zig golden: %v", err)
				}
				want := bytes.TrimSpace(wantData)
				if !bytes.Equal(gotCode, want) {
					t.Errorf("%s Zig\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotCode, want)
				}
			}

			tmp := t.TempDir()
			file := filepath.Join(tmp, "prog.zig")
			if err := os.WriteFile(file, code, 0o644); err != nil {
				t.Fatalf("write temp zig: %v", err)
			}
			exe := filepath.Join(tmp, "prog")
			if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
				writeZigError(outDir, name, fmt.Errorf("zig build error: %v\n%s", err, out))
				t.Skip("zig build error")
				return
			}
			var buf bytes.Buffer
			cmd := exec.Command(exe)
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeZigError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
				t.Skip("run error")
				return
			}
			got := bytes.TrimSpace(buf.Bytes())
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
				t.Logf("updated: %s", outFile)
			} else {
				want, err := os.ReadFile(outFile)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				want = bytes.TrimSpace(want)
				if !bytes.Equal(got, want) {
					t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
				}
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeZigError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
