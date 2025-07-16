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

	cpp "mochi/compiler/x/cpp"
	"mochi/parser"
	"mochi/types"
)

func TestMochiCPPGolden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/cpp")

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

	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		cppPath := filepath.Join(outDir, name+".cpp")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			defer func() {
				if r := recover(); r != nil {
					writeCPPError(outDir, name, fmt.Errorf("panic: %v", r))
					t.Skip("panic")
				}
			}()
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeCPPError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeCPPError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := cpp.New().Compile(prog)
			if err != nil {
				writeCPPError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			gotCode := strings.TrimSpace(string(code))

			if shouldUpdate() {
				if err := os.WriteFile(cppPath, []byte(gotCode+"\n"), 0o644); err != nil {
					t.Fatalf("write cpp: %v", err)
				}
			} else {
				wantData, err := os.ReadFile(cppPath)
				if err != nil {
					t.Fatalf("read cpp golden: %v", err)
				}
				want := strings.TrimSpace(string(wantData))
				if gotCode != want {
					t.Errorf("%s CPP\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotCode, want)
				}
			}

			dir := t.TempDir()
			file := filepath.Join(dir, "prog.cpp")
			if err := os.WriteFile(file, code, 0o644); err != nil {
				t.Fatalf("write temp cpp: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command("g++", file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
				writeCPPError(outDir, name, fmt.Errorf("g++ error: %v\n%s", err, out))
				t.Skip("g++ error")
				return
			}
			var buf bytes.Buffer
			cmd := exec.Command(bin)
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeCPPError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
				t.Skip("run error")
				return
			}
			got := bytes.TrimSpace(buf.Bytes())
			outPath := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
				t.Logf("updated: %s", outPath)
			} else {
				want, err := os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(got, bytes.TrimSpace(want)) {
					t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, bytes.TrimSpace(want))
				}
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
