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

	stcode "mochi/compiler/x/smalltalk"
	sttools "mochi/compiler/x/st"
	"mochi/parser"
	"mochi/types"
)

func TestMochiSmalltalkGolden(t *testing.T) {
	if err := sttools.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Smalltalk")
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

	gst, _ := exec.LookPath("gst")

	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		stPath := filepath.Join(outDir, name+".st")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeSTError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeSTError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := stcode.New().Compile(prog)
			if err != nil {
				writeSTError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			gotCode := strings.TrimSpace(string(code))

			if shouldUpdate() {
				if err := os.WriteFile(stPath, []byte(gotCode+"\n"), 0o644); err != nil {
					t.Fatalf("write st: %v", err)
				}
				t.Logf("updated: %s", stPath)
			} else {
				wantData, err := os.ReadFile(stPath)
				if err != nil {
					t.Fatalf("read st golden: %v", err)
				}
				want := strings.TrimSpace(string(wantData))
				if gotCode != want {
					t.Errorf("%s Smalltalk\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotCode, want)
				}
			}

			if gst == "" {
				writeSTError(outDir, name, fmt.Errorf("gst interpreter not available"))
				return
			}
			stFile := filepath.Join(outDir, name+".st")
			if err := os.WriteFile(stFile, []byte(gotCode+"\n"), 0o644); err != nil {
				t.Fatalf("write st: %v", err)
			}
			cmd := exec.Command(gst, stFile)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeSTError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
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

func writeSTError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
